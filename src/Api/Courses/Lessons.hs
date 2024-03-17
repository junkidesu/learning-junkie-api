{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Courses.Lessons (LessonsAPI, lessonsServer) where

import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database (ensureExists)
import Database.Operations.Courses (courseById)
import Database.Operations.Lessons (allLessons, insertLesson, lessonByNumber)
import Database.PostgreSQL.Simple (Connection, SqlError)
import Servant
import Servant.Auth.Server
import Types.Auth.JWTAuth (JWTAuth)
import qualified Types.Auth.User as AU
import qualified Types.Lesson as L
import qualified Types.Lesson.EditLesson as EL
import qualified Types.Lesson.NewLesson as NL
import Types.User.Role (Role (Admin, Instructor, Student))

type GetLessons =
  Summary "Get all lessons in the course"
    :> Get '[JSON] [L.Lesson]

type GetLessonById =
  Summary "Get lesson in a course by lesson number"
    :> Capture' '[Required, Description "Number of the lesson"] "number" Int
    :> Get '[JSON] L.Lesson

type AddLesson =
  Summary "Add a lesson to a course"
    :> JWTAuth
    :> ReqBody '[JSON] NL.NewLesson
    :> PostCreated '[JSON] L.Lesson

type DeleteLesson =
  Summary "Remove a lesson from a course"
    :> JWTAuth
    :> Capture' '[Required, Description "Number of the lesson"] "number" Int
    :> Verb 'DELETE 204 '[JSON] NoContent

type EditLesson =
  Summary "Edit a lesson in the course"
    :> JWTAuth
    :> Capture' '[Required, Description "Number of the lesson"] "number" Int
    :> ReqBody '[JSON] EL.EditLesson
    :> Put '[JSON] L.Lesson

type LessonsAPI =
  Capture' '[Required, Description "ID of the course"] "id" Int
    :> "lessons"
    :> ( GetLessons
          :<|> GetLessonById
          :<|> AddLesson
          :<|> DeleteLesson
          :<|> EditLesson
       )

lessonsServer :: Pool Connection -> Server LessonsAPI
lessonsServer conns courseId =
  getAllLessons
    :<|> getLessonByNumber
    :<|> addLesson
    :<|> undefined
 where
  -- helper function to throw error if course does not exist
  ensureCourseExists :: Handler ()
  ensureCourseExists = ensureExists conns courseById courseId

  getAllLessons :: Handler [L.Lesson]
  getAllLessons = do
    ensureCourseExists
    liftIO $ allLessons conns courseId

  getLessonByNumber :: Int -> Handler L.Lesson
  getLessonByNumber number = do
    ensureCourseExists

    mbLesson <- liftIO $ lessonByNumber conns courseId number

    case mbLesson of
      Nothing -> throwError err404
      Just lesson -> return lesson

  addLesson :: AuthResult AU.AuthUser -> NL.NewLesson -> Handler L.Lesson
  addLesson (Authenticated authUser) newLesson = do
    case AU.role authUser of
      Student -> throwError err401
      Admin -> addLesson'
      Instructor -> addLesson'
   where
    addLesson' :: Handler L.Lesson
    addLesson' = do
      ensureCourseExists
      result <-
        liftIO $
          try $
            insertLesson conns courseId newLesson ::
          Handler (Either SqlError L.Lesson)

      case result of
        Left _ -> throwError err400
        Right lesson -> return lesson
  addLesson _ _ = throwError err401
