{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Users.Courses (CoursesAPI, coursesServer) where

import Conduit (MonadIO (liftIO))
import Data.Pool (Pool)
import Database (ensureExistsReturning)
import Database.Operations.Users (userById)
import Database.Operations.Users.Courses (taughtCourses)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Types.Course (Course)
import qualified Types.User as U
import Types.User.Role (Role (Instructor))

type GetCourses =
  Summary "Get courses taught by the user"
    :> Capture' '[Required, Description "ID of the user"] "id" Int
    :> "courses"
    :> Get '[JSON] [Course]

type CoursesAPI = GetCourses

coursesServer :: Pool Connection -> Server CoursesAPI
coursesServer conns = getCourses
 where
  getCourses :: Int -> Handler [Course]
  getCourses userId = do
    user <- ensureExistsReturning conns userById userId

    case U.role user of
      Instructor -> liftIO $ taughtCourses conns userId
      _ -> throwError err400
