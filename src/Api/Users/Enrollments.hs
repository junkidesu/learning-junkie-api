{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Users.Enrollments (EnrollmentsAPI, enrollmentsServer) where

import Certificate (generateCertificateBS)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Data.Text (Text)
import Database (ensureExists)
import Database.Operations.Courses.Completions (courseCompletion)
import Database.Operations.Users (userById)
import Database.Operations.Users.Courses (userCoursesById)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Types.Course (Course)
import Types.PDF (PDF (PDF))

type GetUserCourses =
  Summary "Get all courses that a user is enrolled in"
    :> Capture' '[Required, Description "ID of the user"] "id" Int
    :> "enrollments"
    :> Get '[JSON] [Course]

type GetCertificate =
  Summary "Get certificate for course"
    :> Capture' '[Required, Description "ID of the user"] "id" Int
    :> "enrollments"
    :> Capture' '[Required, Description "ID of the course"] "courseId" Int
    :> "certificate"
    :> Get '[OctetStream] (Headers '[Header "content-disposition" Text] PDF)

type EnrollmentsAPI =
  GetUserCourses
    :<|> GetCertificate

enrollmentsServer :: Pool Connection -> Server EnrollmentsAPI
enrollmentsServer conns = getUserEnrollments :<|> getCertificate
 where
  getUserEnrollments :: Int -> Handler [Course]
  getUserEnrollments userId = do
    ensureExists conns userById userId

    liftIO $ userCoursesById conns userId

  getCertificate :: Server GetCertificate
  getCertificate userId courseId = do
    mbCompletion <- liftIO $ courseCompletion conns courseId userId

    case mbCompletion of
      Nothing -> throwError err404
      Just completion ->
        return $
          addHeader "attachment; filename = certificate.pdf" $
            PDF $
              generateCertificateBS completion
