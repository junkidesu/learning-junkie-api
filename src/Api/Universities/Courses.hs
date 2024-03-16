{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Universities.Courses (CoursesAPI, coursesServer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database.Operations.Courses (insertCourse, universityCoursesById)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Auth.Server
import Types.Auth.JWTAuth
import qualified Types.Auth.User as AU
import Types.Course (Course)
import qualified Types.Course.NewCourse as NC
import Types.User.Role (Role (Admin))

type AddCourse =
  Summary "Add course to the university"
    :> JWTAuth
    :> ReqBody '[JSON] NC.NewCourse
    :> PostCreated '[JSON] Course

type GetCourses =
  Summary "Get courses by university with given ID"
    :> Get '[JSON] [Course]

type CoursesAPI =
  Capture' '[Required, Description "ID of the university"] "id" Int
    :> "courses"
    :> (AddCourse :<|> GetCourses)

coursesServer :: Pool Connection -> Server CoursesAPI
coursesServer conns universityId = addCourse :<|> getCourses
 where
  addCourse :: AuthResult AU.AuthUser -> NC.NewCourse -> Handler Course
  addCourse (Authenticated authUser) newCourse =
    case AU.role authUser of
      Admin -> liftIO $ insertCourse conns universityId newCourse
      _ -> throwError err401
  addCourse _ _ = throwError err401

  getCourses :: Handler [Course]
  getCourses = liftIO $ universityCoursesById conns universityId
