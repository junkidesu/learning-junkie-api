{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Api.Exercises.Questions (QuestionsAPI, questionsServer) where

import Control.Exception (try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database (ensureExistsReturning)
import Database.Operations.Courses.Completions (courseCompletion, insertCompletion)
import Database.Operations.Courses.Enrollments (userIsEnrolled)
import Database.Operations.Exercises.Questions (questionById, updateQuestion)
import Database.Operations.Exercises.Solutions (insertSolution, questionSolution, userDidSolve)
import Database.Operations.Users.Progress (userCourseProgress)
import Database.PostgreSQL.Simple (Connection, SqlError)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))
import Types.Auth.JWTAuth (JWTAuth)
import qualified Types.Auth.User as AU
import qualified Types.Course as C
import qualified Types.Exercise.EditQuestion as EQ
import qualified Types.Exercise.Question as Q
import Types.Solution.ExerciseCheck
import qualified Types.Solution.Question as QS
import Types.Solution.Response (Response (Response))
import Types.Solvable (Solvable (checkSolution))
import qualified Types.User as U
import Types.User.Progress (courseFinished)
import Types.User.Role (Role (..))

type GetQuestionById =
  Summary "Get question ID"
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> Get '[JSON] Q.Question

type EditQuestion =
  Summary "Edit question by ID"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> ReqBody '[JSON] EQ.EditQuestion
    :> Put '[JSON] Q.Question

type PostSolution =
  Summary "Post solution to question"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> "solution"
    :> ReqBody '[JSON] QS.QuestionSolution
    :> Post '[JSON] Response

type GetSolution =
  Summary "View solution to question"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> "solution"
    :> Get '[JSON] QS.QuestionSolution

type QuestionsAPI =
  GetQuestionById
    :<|> EditQuestion
    :<|> PostSolution
    :<|> GetSolution

questionsServer :: Pool Connection -> Server QuestionsAPI
questionsServer conns =
  getQuestionById
    :<|> editQuestionById
    :<|> postSolution
    :<|> getSolution
 where
  getQuestionById :: Int -> Handler Q.Question
  getQuestionById questionId = do
    mbQuestion <- liftIO $ questionById conns questionId

    case mbQuestion of
      Nothing -> throwError err404
      Just question -> return question

  editQuestionById :: AuthResult AU.AuthUser -> Int -> EQ.EditQuestion -> Handler Q.Question
  editQuestionById (Authenticated authUser) questionId editQuestion =
    case AU.role authUser of
      Admin -> do
        res <- liftIO $ try $ updateQuestion conns questionId editQuestion :: Handler (Either SqlError (Maybe Q.Question))

        case res of
          Left _ -> throwError err400
          Right mbQuestion -> case mbQuestion of
            Nothing -> throwError err404
            Just question -> return question
      Instructor -> do
        currentQuestion <- ensureExistsReturning conns questionById questionId

        when
          (AU.id authUser /= (U.id . C.instructor . Q.course $ currentQuestion))
          $ throwError err401

        res <- liftIO $ try $ updateQuestion conns questionId editQuestion :: Handler (Either SqlError (Maybe Q.Question))

        case res of
          Left _ -> throwError err400
          Right mbQuestion -> case mbQuestion of
            Nothing -> throwError err404
            Just question -> return question
      Student -> throwError err401
  editQuestionById _ _ _ = throwError err401

  getSolution :: AuthResult AU.AuthUser -> Int -> Handler QS.QuestionSolution
  getSolution (Authenticated authUser) questionId =
    case AU.role authUser of
      Admin -> do
        mbSolution <- liftIO $ questionSolution conns questionId

        case mbSolution of
          Nothing -> throwError err404
          Just solution -> return solution
      _ -> do
        userSolved <- liftIO $ userDidSolve conns (AU.id authUser) questionId

        unless userSolved $ throwError err401

        mbSolution <- liftIO $ questionSolution conns questionId

        case mbSolution of
          Nothing -> throwError err404
          Just solution -> return solution
  getSolution _ _ = throwError err401

  postSolution :: AuthResult AU.AuthUser -> Int -> QS.QuestionSolution -> Handler Response
  postSolution (Authenticated authUser) questionId solution = do
    question <- ensureExistsReturning conns questionById questionId

    let res = checkSolution question solution
        courseId = C.id . Q.course $ question
        userId = AU.id authUser

    isEnrolled <- liftIO $ userIsEnrolled conns courseId userId

    unless isEnrolled (throwError err401)

    case res of
      ExerciseSuccess -> do
        grade <-
          liftIO $
            insertSolution
              conns
              (AU.id authUser)
              questionId
              (Q.grade question)

        mbCompletion <- liftIO $ courseCompletion conns courseId userId

        case mbCompletion of
          Nothing -> do
            mbProgress <- liftIO $ userCourseProgress conns userId courseId

            case mbProgress of
              Nothing -> throwError err404
              Just progress -> when (courseFinished progress) (liftIO $ insertCompletion conns courseId userId)
          Just _ -> return ()

        return $ Response ExerciseSuccess (Just grade)
      ExerciseFailure -> return $ Response ExerciseFailure Nothing
      ExercisePending -> return $ Response ExercisePending Nothing
  postSolution _ _ _ = throwError err401
