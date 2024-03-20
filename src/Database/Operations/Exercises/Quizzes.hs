module Database.Operations.Exercises.Quizzes (allQuizzes, insertQuiz) where

import Data.Pool (Pool)
import Database (getMany, insertReturning)
import Database.PostgreSQL.Simple (Connection)
import Database.Queries.Exercises.Quizzes (allQuizzesQ, insertQuizQ)
import Types.Exercise.Choice (Choice (A, B, C, D))
import qualified Types.Exercise.NewQuiz as NQ
import Types.Exercise.Quiz (Quiz)

allQuizzes :: Pool Connection -> Int -> Int -> IO [Quiz]
allQuizzes conns courseId lessonNumber = getMany conns allQuizzesQ (courseId, lessonNumber)

insertQuiz :: Pool Connection -> Int -> Int -> NQ.NewQuiz -> IO Quiz
insertQuiz conns courseId lessonNumber newQuiz =
        insertReturning
                conns
                insertQuizQ
                ( NQ.grade newQuiz
                , courseId
                , lessonNumber
                , NQ.question newQuiz
                , NQ.options newQuiz A
                , NQ.options newQuiz B
                , NQ.options newQuiz C
                , NQ.options newQuiz D
                , NQ.correct newQuiz
                )
