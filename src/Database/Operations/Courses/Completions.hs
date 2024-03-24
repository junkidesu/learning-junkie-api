module Database.Operations.Courses.Completions (courseCompletion, insertCompletion) where

import Data.Pool (Pool)
import Database (getOne, insert)
import Database.PostgreSQL.Simple (Connection)
import Database.Queries.Courses.Completions (courseCompletionQ, insertCompletionQ)
import Types.Course.Completion (Completion)

courseCompletion :: Pool Connection -> Int -> Int -> IO (Maybe Completion)
courseCompletion conns courseId userId = getOne conns courseCompletionQ (courseId, userId)

insertCompletion :: Pool Connection -> Int -> Int -> IO ()
insertCompletion conns courseId userId = insert conns insertCompletionQ (courseId, userId)
