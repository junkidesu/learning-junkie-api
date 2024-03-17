{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Universities.Courses (CoursesAPI, coursesServer) where

import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database (ensureExists)
import Database.Operations.Courses (insertCourse, universityCoursesById)
import Database.Operations.Universities (universityById)
import Database.PostgreSQL.Simple (Connection, SqlError)
import Servant
import Servant.Auth.Server
import Types.Auth.JWTAuth
import qualified Types.Auth.User as AU
import Types.Course (Course)
import qualified Types.Course.NewCourse as NC

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
  ensureUniversityExists :: Handler ()
  ensureUniversityExists = ensureExists conns universityById universityId

  addCourse :: AuthResult AU.AuthUser -> NC.NewCourse -> Handler Course
  addCourse (Authenticated authUser) newCourse = do
    requireAdmin authUser
    ensureUniversityExists
    result <-
      liftIO $
        try $
          insertCourse conns universityId newCourse ::
        Handler (Either SqlError Course)

    case result of
      Left _ -> throwError err400
      Right course -> return course
  addCourse _ _ = throwError err401

  getCourses :: Handler [Course]
  getCourses = do
    ensureUniversityExists
    liftIO $ universityCoursesById conns universityId
