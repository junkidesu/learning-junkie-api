module Database.Operations.Users.Progress (userProgress) where

import Data.Pool (Pool)
import Database (getMany)
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.Queries.Users.Progress (userProgressQ)
import Types.User.Progress (Progress)

userProgress :: Pool Connection -> Int -> IO [Progress]
userProgress conns userId = getMany conns userProgressQ (Only userId)
