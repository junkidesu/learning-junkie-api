{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Courses where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database.Operations.Universities (allUniversities, insertUniversity)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Auth
import Servant.Auth.Server
import qualified Types.Auth.User as AU
import Types.Course (Course)

type JWTAuth = Auth '[JWT] AU.AuthUser

type GetAllCourses =
  Summary "Get all courses"
    :> Get '[JSON] [Course]

type GetCourseById =
  Summary "Get course by ID"
    :> Capture' '[Required, Description "ID of the course"] "id" Int
    :> Get '[JSON] Course

type DeleteCourseById =
  Summary "Delete course by ID"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the course"] "id" Int
    :> Verb 'DELETE 204 '[JSON] NoContent

type CoursesAPI = "courses" :> (GetAllCourses :<|> GetCourseById :<|> DeleteCourseById)

coursesServer :: Pool Connection -> Server CoursesAPI
coursesServer conns = getAllCourses :<|> getCourseById :<|> deleteCourseById
 where
  getAllCourses :: Handler [Course]
  getAllCourses = undefined

  getCourseById :: Int -> Handler Course
  getCourseById courseId = undefined

  deleteCourseById :: AuthResult AU.AuthUser -> Int -> Handler NoContent
  deleteCourseById (Authenticated authUser) courseId = undefined
  deleteCourseById _ _ = throwError err401
