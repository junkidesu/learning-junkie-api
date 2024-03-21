{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Exercises.Questions (QuestionsAPI, questionsServer) where

import Control.Exception (try)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database (ensureExistsReturning)
import Database.Operations.Exercises (exerciseById)
import Database.Operations.Exercises.Questions (questionById, updateQuestion)
import Database.PostgreSQL.Simple (Connection, SqlError)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))
import Types.Auth.JWTAuth (JWTAuth)
import qualified Types.Auth.User as AU
import qualified Types.Course as C
import qualified Types.Exercise as E
import qualified Types.Exercise.EditQuestion as EQ
import Types.Exercise.Question (Question)
import qualified Types.User as U
import Types.User.Role

type GetQuestionById =
  Summary "Get question by ID"
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> Get '[JSON] Question

type EditQuestion =
  Summary "Edit question with given ID"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> ReqBody '[JSON] EQ.EditQuestion
    :> Put '[JSON] Question

type QuestionsAPI =
  "questions"
    :> (GetQuestionById :<|> EditQuestion)

questionsServer :: Pool Connection -> Server QuestionsAPI
questionsServer conns = getQuestionById :<|> editQuestion
 where
  getQuestionById :: Int -> Handler Question
  getQuestionById exerciseId = do
    mbQuestion <- liftIO $ questionById conns exerciseId

    case mbQuestion of
      Nothing -> throwError err404
      Just question -> return question
  editQuestion :: AuthResult AU.AuthUser -> Int -> EQ.EditQuestion -> Handler Question
  editQuestion (Authenticated authUser) exerciseId eq =
    case AU.role authUser of
      Admin -> editQuestion'
      Instructor -> do
        exercise <- ensureExistsReturning conns exerciseById exerciseId

        when (AU.id authUser /= (U.id . C.instructor . E.course $ exercise)) (throwError err401)

        editQuestion'
      Student -> throwError err401
   where
    editQuestion' = do
      res <-
        liftIO $
          try $
            updateQuestion conns exerciseId eq ::
          Handler (Either SqlError (Maybe Question))

      case res of
        Left _ -> throwError err400
        Right mbQuestion ->
          case mbQuestion of
            Nothing -> throwError err404
            Just question -> return question
  editQuestion _ _ _ = throwError err401
