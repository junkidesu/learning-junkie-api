{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Courses.Exercises.Quizzes (QuizzesAPI, quizzesServer) where

import Control.Exception (try)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database (ensureExists, ensureExistsReturning)
import Database.Operations.Courses (courseById)
import Database.Operations.Exercises.Quizzes
import Database.Operations.Lessons (lessonByNumber)
import Database.PostgreSQL.Simple (Connection, SqlError)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))
import Types.Auth.JWTAuth (JWTAuth)
import qualified Types.Auth.User as AU
import qualified Types.Course as C
import qualified Types.Exercise.NewQuiz as NQ
import Types.Exercise.Quiz (Quiz)
import qualified Types.User as U
import Types.User.Role (Role (Admin, Instructor, Student))

type GetAllQuizzes =
  Summary "Get all quizzes in a lesson"
    :> Get '[JSON] [Quiz]

type AddQuiz =
  Summary "Add a quiz to a lesson"
    :> JWTAuth
    :> ReqBody '[JSON] NQ.NewQuiz
    :> PostCreated '[JSON] Quiz

type QuizzesAPI =
  Capture' '[Required, Description "Number of the lesson"] "number" Int
    :> "quizzes"
    :> ( GetAllQuizzes
          :<|> AddQuiz
       )

quizzesServer :: Pool Connection -> Int -> Server QuizzesAPI
quizzesServer conns courseId lessonNumber =
  getAllQuizzes
    :<|> addQuiz
 where
  ensureCourseExists = ensureExists conns courseById courseId
  ensureLessonExists = ensureExists conns (`lessonByNumber` courseId) lessonNumber
  getCurrentCourse = ensureExistsReturning conns courseById courseId

  getAllQuizzes :: Handler [Quiz]
  getAllQuizzes = do
    ensureCourseExists
    ensureLessonExists
    liftIO $ allQuizzes conns courseId lessonNumber

  addQuiz :: AuthResult AU.AuthUser -> NQ.NewQuiz -> Handler Quiz
  addQuiz (Authenticated authUser) newQuiz =
    case AU.role authUser of
      Admin -> do
        ensureCourseExists
        ensureLessonExists
        addQuiz'
      Instructor -> do
        course <- getCurrentCourse

        when (AU.id authUser /= (U.id . C.instructor $ course)) (throwError err401)

        addQuiz'
      Student -> throwError err401
   where
    addQuiz' = do
      res <-
        liftIO $
          try $
            insertQuiz conns courseId lessonNumber newQuiz ::
          Handler (Either SqlError Quiz)

      case res of
        Left _ -> throwError err400
        Right quiz -> return quiz
  addQuiz _ _ = throwError err401
