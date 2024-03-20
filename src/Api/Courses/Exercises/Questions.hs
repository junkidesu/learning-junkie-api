{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Courses.Exercises.Questions (QuestionsAPI, questionsServer) where

import Control.Exception (try)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database (ensureExists, ensureExistsReturning)
import Database.Operations.Courses (courseById)
import Database.Operations.Exercises.Questions (allQuestions, insertQuestion)
import Database.Operations.Lessons (lessonByNumber)
import Database.PostgreSQL.Simple (Connection, SqlError)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))
import Types.Auth.JWTAuth (JWTAuth)
import qualified Types.Auth.User as AU
import qualified Types.Course as C
import qualified Types.Exercise.NewQuestion as NQ
import Types.Exercise.Question (Question)
import qualified Types.User as U
import Types.User.Role (Role (Admin, Instructor, Student))

type GetAllQuestions =
  Summary "Get all questions in a lesson"
    :> Get '[JSON] [Question]

type AddQuestion =
  Summary "Add a question to a lesson"
    :> JWTAuth
    :> ReqBody '[JSON] NQ.NewQuestion
    :> PostCreated '[JSON] Question

type QuestionsAPI =
  Capture' '[Required, Description "Number of the lesson"] "number" Int
    :> "questions"
    :> ( GetAllQuestions
          :<|> AddQuestion
       )

questionsServer :: Pool Connection -> Int -> Server QuestionsAPI
questionsServer conns courseId lessonNumber =
  getAllQuestions
    :<|> addQuestion
 where
  ensureCourseExists = ensureExists conns courseById courseId
  ensureLessonExists = ensureExists conns (`lessonByNumber` courseId) lessonNumber
  getCurrentCourse = ensureExistsReturning conns courseById courseId

  getAllQuestions :: Handler [Question]
  getAllQuestions = do
    ensureCourseExists
    ensureLessonExists
    liftIO $ allQuestions conns courseId lessonNumber

  addQuestion :: AuthResult AU.AuthUser -> NQ.NewQuestion -> Handler Question
  addQuestion (Authenticated authUser) newQuestion =
    case AU.role authUser of
      Admin -> do
        ensureCourseExists
        ensureLessonExists
        addQuestion'
      Instructor -> do
        course <- getCurrentCourse

        when (AU.id authUser /= (U.id . C.instructor $ course)) (throwError err401)

        addQuestion'
      Student -> throwError err401
   where
    addQuestion' = do
      res <-
        liftIO $
          try $
            insertQuestion conns courseId lessonNumber newQuestion ::
          Handler (Either SqlError Question)

      case res of
        Left _ -> throwError err400
        Right question -> return question
  addQuestion _ _ = throwError err401
