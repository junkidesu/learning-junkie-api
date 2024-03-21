{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Exercises.Quizzes (QuizzesAPI, quizzesServer) where

import Control.Exception (try)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database (ensureExistsReturning)
import Database.Operations.Exercises (exerciseById)
import Database.Operations.Exercises.Quizzes (quizById, updateQuiz)
import Database.PostgreSQL.Simple (Connection, SqlError)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))
import Types.Auth.JWTAuth (JWTAuth)
import qualified Types.Auth.User as AU
import qualified Types.Course as C
import qualified Types.Exercise as E
import qualified Types.Exercise.EditQuiz as EQ
import Types.Exercise.Quiz (Quiz)
import qualified Types.User as U
import Types.User.Role

type GetQuizById =
  Summary "Get quiz by ID"
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> Get '[JSON] Quiz

type EditQuiz =
  Summary "Edit quiz with given ID"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> ReqBody '[JSON] EQ.EditQuiz
    :> Put '[JSON] Quiz

type QuizzesAPI =
  "quizzes"
    :> (GetQuizById :<|> EditQuiz)

quizzesServer :: Pool Connection -> Server QuizzesAPI
quizzesServer conns =
  getQuizById
    :<|> editQuiz
 where
  getQuizById :: Int -> Handler Quiz
  getQuizById exerciseId = do
    mbQuiz <- liftIO $ quizById conns exerciseId

    case mbQuiz of
      Nothing -> throwError err404
      Just quiz -> return quiz
  editQuiz :: AuthResult AU.AuthUser -> Int -> EQ.EditQuiz -> Handler Quiz
  editQuiz (Authenticated authUser) exerciseId eq =
    case AU.role authUser of
      Admin -> editQuiz'
      Instructor -> do
        exercise <- ensureExistsReturning conns exerciseById exerciseId

        when (AU.id authUser /= (U.id . C.instructor . E.course $ exercise)) (throwError err401)

        editQuiz'
      Student -> throwError err401
   where
    editQuiz' = do
      res <-
        liftIO $
          try $
            updateQuiz conns exerciseId eq ::
          Handler (Either SqlError (Maybe Quiz))

      case res of
        Left _ -> throwError err400
        Right mbQuiz ->
          case mbQuiz of
            Nothing -> throwError err404
            Just quiz -> return quiz
  editQuiz _ _ _ = throwError err401
