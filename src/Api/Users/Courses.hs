{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Users.Courses (CoursesAPI, coursesServer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database (ensureExists)
import Database.Operations.Users (userById)
import Database.Operations.Users.Courses (userCoursesById)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Types.Course (Course)

type GetUserCourses =
  Summary "Get all courses that a user is enrolled in"
    :> Capture' '[Required, Description "ID of the user"] "id" Int
    :> "courses"
    :> Get '[JSON] [Course]

type CoursesAPI = GetUserCourses

coursesServer :: Pool Connection -> Server CoursesAPI
coursesServer conns = getUserCourses
 where
  getUserCourses :: Int -> Handler [Course]
  getUserCourses userId = do
    ensureExists conns userById userId

    liftIO $ userCoursesById conns userId
