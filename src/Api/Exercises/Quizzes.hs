{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Api.Exercises.Quizzes (QuizzesAPI, quizzesServer) where

import Control.Exception (try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database (ensureExistsReturning)
import Database.Operations.Courses.Completions (insertCompletion)
import Database.Operations.Courses.Enrollments (userIsEnrolled)
import Database.Operations.Exercises.Quizzes (quizById, updateQuiz)
import Database.Operations.Exercises.Solutions (insertSolution, quizSolution, userDidSolve)
import Database.Operations.Users.Progress (userCourseProgress)
import Database.PostgreSQL.Simple (Connection, SqlError)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))
import Types.Auth.JWTAuth (JWTAuth)
import qualified Types.Auth.User as AU
import qualified Types.Course as C
import qualified Types.Exercise.EditQuiz as EQ
import qualified Types.Exercise.Quiz as Q
import Types.Solution.ExerciseCheck
import qualified Types.Solution.Quiz as QS
import Types.Solution.Response (Response (Response))
import Types.Solvable (Solvable (checkSolution))
import qualified Types.User as U
import Types.User.Progress (courseFinished)
import Types.User.Role (Role (..))

type GetQuizById =
  Summary "Get quiz ID"
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> Get '[JSON] Q.Quiz

type EditQuiz =
  Summary "Edit quiz by ID"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> ReqBody '[JSON] EQ.EditQuiz
    :> Put '[JSON] Q.Quiz

type PostSolution =
  Summary "Post solution to quiz"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> "solution"
    :> ReqBody '[JSON] QS.QuizSolution
    :> Post '[JSON] Response

type GetSolution =
  Summary "View solution to quiz"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> "solution"
    :> Get '[JSON] QS.QuizSolution

type QuizzesAPI =
  GetQuizById
    :<|> EditQuiz
    :<|> PostSolution
    :<|> GetSolution

quizzesServer :: Pool Connection -> Server QuizzesAPI
quizzesServer conns =
  getQuizById
    :<|> editQuizById
    :<|> postSolution
    :<|> getSolution
 where
  getQuizById :: Int -> Handler Q.Quiz
  getQuizById quizId = do
    mbQuestion <- liftIO $ quizById conns quizId

    case mbQuestion of
      Nothing -> throwError err404
      Just question -> return question

  editQuizById :: AuthResult AU.AuthUser -> Int -> EQ.EditQuiz -> Handler Q.Quiz
  editQuizById (Authenticated authUser) quizId editQuiz =
    case AU.role authUser of
      Admin -> do
        res <- liftIO $ try $ updateQuiz conns quizId editQuiz :: Handler (Either SqlError (Maybe Q.Quiz))

        case res of
          Left _ -> throwError err400
          Right mbQuiz -> case mbQuiz of
            Nothing -> throwError err404
            Just quiz -> return quiz
      Instructor -> do
        currentQuiz <- ensureExistsReturning conns quizById quizId

        when
          (AU.id authUser /= (U.id . C.instructor . Q.course $ currentQuiz))
          $ throwError err401

        res <- liftIO $ try $ updateQuiz conns quizId editQuiz :: Handler (Either SqlError (Maybe Q.Quiz))

        case res of
          Left _ -> throwError err400
          Right mbQuiz -> case mbQuiz of
            Nothing -> throwError err404
            Just quiz -> return quiz
      Student -> throwError err401
  editQuizById _ _ _ = throwError err401

  getSolution :: AuthResult AU.AuthUser -> Int -> Handler QS.QuizSolution
  getSolution (Authenticated authUser) quizId =
    case AU.role authUser of
      Admin -> do
        mbSolution <- liftIO $ quizSolution conns quizId

        case mbSolution of
          Nothing -> throwError err404
          Just solution -> return solution
      _ -> do
        userSolved <- liftIO $ userDidSolve conns (AU.id authUser) quizId

        unless userSolved $ throwError err401

        mbSolution <- liftIO $ quizSolution conns quizId

        case mbSolution of
          Nothing -> throwError err404
          Just solution -> return solution
  getSolution _ _ = throwError err401

  postSolution :: AuthResult AU.AuthUser -> Int -> QS.QuizSolution -> Handler Response
  postSolution (Authenticated authUser) quizId solution = do
    quiz <- ensureExistsReturning conns quizById quizId

    let res = checkSolution quiz solution
        courseId = C.id . Q.course $ quiz
        userId = AU.id authUser

    isEnrolled <- liftIO $ userIsEnrolled conns courseId userId

    unless isEnrolled (throwError err401)

    case res of
      ExerciseSuccess -> do
        liftIO $
          insertSolution
            conns
            (AU.id authUser)
            quizId
            (Q.grade quiz)
        mbProgress <- liftIO $ userCourseProgress conns userId courseId

        case mbProgress of
          Nothing -> throwError err404
          Just progress -> when (courseFinished progress) (liftIO $ insertCompletion conns courseId userId)

        return $ Response ExerciseSuccess (Just (Q.grade quiz))
      ExerciseFailure -> return $ Response ExerciseFailure Nothing
      ExercisePending -> return $ Response ExercisePending Nothing
  postSolution _ _ _ = throwError err401
