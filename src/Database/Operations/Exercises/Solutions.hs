module Database.Operations.Exercises.Solutions (
        insertSolution,
        userDidSolve,
        questionSolution,
        essayModelSolution,
        quizSolution,
) where

import Data.Pool (Pool)
import Database (getOne, insertReturning)
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.Queries.Exercises.Solutions (essayModelSolutionQ, insertSolutionQ, questionSolutionQ, quizSolutionQ, userDidSolveQ)
import Types.Solution.Essay (EssaySolution)
import Types.Solution.Question (QuestionSolution)
import Types.Solution.Quiz (QuizSolution)
import Types.User (User)

insertSolution :: Pool Connection -> Int -> Int -> Int -> IO Int
insertSolution conns userId exerciseId grade = do
        (g : _) <- insertReturning conns insertSolutionQ (userId, exerciseId, grade)
        return g

userDidSolve :: Pool Connection -> Int -> Int -> IO Bool
userDidSolve conns userId exerciseId = do
        res <- getOne conns userDidSolveQ (exerciseId, userId) :: IO (Maybe User)

        case res of
                Nothing -> pure False
                Just _ -> pure True

questionSolution :: Pool Connection -> Int -> IO (Maybe QuestionSolution)
questionSolution conns exerciseId = getOne conns questionSolutionQ (Only exerciseId)

essayModelSolution :: Pool Connection -> Int -> IO (Maybe EssaySolution)
essayModelSolution conns exerciseId = getOne conns essayModelSolutionQ (Only exerciseId)

quizSolution :: Pool Connection -> Int -> IO (Maybe QuizSolution)
quizSolution conns exerciseId = getOne conns quizSolutionQ (Only exerciseId)
