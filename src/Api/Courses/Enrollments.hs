{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Courses.Enrollments (EnrollmentsAPI, enrollmentsServer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database.Operations.Courses.Enrollments (enrollUserInCourse, usersEnrolledInCourse)
import Database.PostgreSQL.Simple (Connection)
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
  enrollInCourse :: AuthResult AU.AuthUser -> Handler NoContent
  enrollInCourse (Authenticated authUser) = do
    liftIO $ enrollUserInCourse conns (AU.id authUser) courseId
    return NoContent
  enrollInCourse _ = throwError err401

  enrolledUsers :: Handler [User]
  enrolledUsers = liftIO $ usersEnrolledInCourse conns courseId
