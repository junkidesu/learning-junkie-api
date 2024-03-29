module Database.Operations.Users.Progress (userProgress, userProgressByCourse) where

import Data.Pool (Pool)
import Database (getMany, getOne)
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.Queries.Users.Progress (userProgressByCourseQ, userProgressQ)
import Types.User.Progress (Progress)

userProgress :: Pool Connection -> Int -> IO [Progress]
userProgress conns userId = getMany conns userProgressQ (Only userId)

userProgressByCourse :: Pool Connection -> Int -> Int -> IO (Maybe Progress)
userProgressByCourse conns userId courseId = getOne conns userProgressByCourseQ (userId, courseId)
