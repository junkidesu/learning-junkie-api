module Database.Operations.Exercises (exerciseById, deleteExercise) where

import Control.Monad (void)
import Data.Pool (Pool)
import Database (delete, getOne)
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.Queries.Exercises (deleteExerciseQ, exerciseByIdQ)
import Types.Exercise (Exercise)

exerciseById :: Pool Connection -> Int -> IO (Maybe Exercise)
exerciseById conns exerciseId = getOne conns exerciseByIdQ (Only exerciseId)

deleteExercise :: Pool Connection -> Int -> IO ()
deleteExercise conns exerciseId = void $ delete conns deleteExerciseQ (Only exerciseId)
