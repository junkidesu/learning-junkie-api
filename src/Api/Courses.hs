{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Courses (CoursesAPI, coursesServer) where

import Api.Courses.Banner (BannerAPI, bannerServer)
import Api.Courses.Enrollments (EnrollmentsAPI, enrollmentsServer)
import Api.Courses.Lessons (LessonsAPI, lessonsServer)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database.Operations.Courses (allCourses, courseById, deleteCourse)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Auth.Server
import Types.Auth.JWTAuth
import qualified Types.Auth.User as AU
import Types.Course (Course)
import Upload.Environment (S3Environment)

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

type CoursesAPI =
  "courses"
    :> ( GetAllCourses
          :<|> GetCourseById
          :<|> DeleteCourseById
          :<|> EnrollmentsAPI
          :<|> LessonsAPI
          :<|> BannerAPI
       )

coursesServer :: Pool Connection -> S3Environment -> Server CoursesAPI
coursesServer conns s3env =
  getAllCourses
    :<|> getCourseById
    :<|> deleteCourseById
    :<|> enrollmentsServer conns
    :<|> lessonsServer conns
    :<|> bannerServer conns s3env
 where
  getAllCourses :: Handler [Course]
  getAllCourses = liftIO $ allCourses conns

  getCourseById :: Int -> Handler Course
  getCourseById courseId = do
    mbCourse <- liftIO $ courseById conns courseId

    case mbCourse of
      Nothing -> throwError err404
      Just course -> return course

  deleteCourseById :: AuthResult AU.AuthUser -> Int -> Handler NoContent
  deleteCourseById (Authenticated authUser) courseId = do
    requireAdmin authUser
    liftIO $ deleteCourse conns courseId
    return NoContent
  deleteCourseById _ _ = throwError err401
