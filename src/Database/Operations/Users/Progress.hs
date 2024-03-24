module Database.Operations.Users.Progress (userProgress, userCourseProgress) where

import Data.Pool (Pool)
import Database (getMany, getOne)
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.Queries.Users.Progress (userCourseProgressQ, userProgressQ)
import Types.User.Progress (Progress)

userProgress :: Pool Connection -> Int -> IO [Progress]
userProgress conns userId = getMany conns userProgressQ (Only userId)

userCourseProgress :: Pool Connection -> Int -> Int -> IO (Maybe Progress)
userCourseProgress conns userId courseId = getOne conns userCourseProgressQ (userId, courseId)
