module Database.Operations.Exercises.Solutions (insertSolution) where

import Data.Pool (Pool)
import Database (insert)
import Database.PostgreSQL.Simple (Connection)
import Database.Queries.Exercises.Solutions (insertSolutionQ)

insertSolution :: Pool Connection -> Int -> Int -> IO ()
insertSolution conns userId exerciseId = insert conns insertSolutionQ (userId, exerciseId)
