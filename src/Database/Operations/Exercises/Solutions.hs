module Database.Operations.Exercises.Solutions (insertSolution) where

import Data.Pool (Pool)
import Database (insertReturning)
import Database.PostgreSQL.Simple (Connection)
import Database.Queries.Exercises.Solutions (insertSolutionQ)

insertSolution :: Pool Connection -> Int -> Int -> Int -> IO Int
insertSolution conns userId exerciseId grade = do
        (g : _) <- insertReturning conns insertSolutionQ (userId, exerciseId, grade)
        return g
