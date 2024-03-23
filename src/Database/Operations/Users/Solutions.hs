module Database.Operations.Users.Solutions (userSolutions) where

import Data.Pool (Pool)
import Database (getMany)
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.Queries.Users.Solutions (userSolutionsQ)
import Types.Exercise (Exercise)

userSolutions :: Pool Connection -> Int -> IO [Exercise]
userSolutions conns userId = getMany conns userSolutionsQ (Only userId)
