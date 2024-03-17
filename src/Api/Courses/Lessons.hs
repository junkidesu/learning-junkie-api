{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Courses.Lessons (LessonsAPI, lessonsServer) where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Types.Auth.JWTAuth (JWTAuth)
import qualified Types.Lesson as L
import qualified Types.Lesson.EditLesson as EL

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
lessonsServer _ = undefined
