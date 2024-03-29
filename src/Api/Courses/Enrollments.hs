{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Courses.Enrollments (EnrollmentsAPI, enrollmentsServer) where

import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database (ensureExists)
import Database.Operations.Courses (courseById)
import Database.Operations.Courses.Enrollments (enrollUserInCourse, usersEnrolledInCourse)
import Database.PostgreSQL.Simple (Connection, SqlError)
import Servant
import Servant.Auth.Server
import Types.Auth.JWTAuth
import qualified Types.Auth.User as AU
import Types.User (User)

type EnrollInCourse =
  Summary "Enroll a user in a course"
    :> JWTAuth
    :> Verb 'POST 200 '[JSON] NoContent

type EnrolledUsers =
  Summary "Users enrolled in the course"
    :> Get '[JSON] [User]

type EnrollmentsAPI =
  Capture' '[Required, Description "ID of the course"] "id" Int
    :> "enrollments"
    :> (EnrollInCourse :<|> EnrolledUsers)

enrollmentsServer :: Pool Connection -> Server EnrollmentsAPI
enrollmentsServer conns courseId = enrollInCourse :<|> enrolledUsers
 where
  -- helper function to throw error if course does not exist
  ensureCourseExists :: Handler ()
  ensureCourseExists = ensureExists conns courseById courseId

  enrollInCourse :: AuthResult AU.AuthUser -> Handler NoContent
  enrollInCourse (Authenticated authUser) = do
    ensureCourseExists

    result <- liftIO $ try $ enrollUserInCourse conns (AU.id authUser) courseId :: Handler (Either SqlError ())
    case result of
      Left _ -> throwError err400
      Right _ -> return NoContent
  enrollInCourse _ = throwError err401

  enrolledUsers :: Handler [User]
  enrolledUsers = do
    ensureCourseExists
    liftIO $ usersEnrolledInCourse conns courseId
