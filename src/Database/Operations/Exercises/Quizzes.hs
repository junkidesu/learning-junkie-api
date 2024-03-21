module Database.Operations.Exercises.Quizzes (
        allQuizzes,
        quizById,
        insertQuiz,
        updateQuiz,
) where

import Data.Pool (Pool)
import Database (getMany, getOne, insertReturning, updateReturning)
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.Queries.Exercises.Quizzes (allQuizzesQ, insertQuizQ, quizByIdQ, updateQuizQ)
import Types.Exercise.Choice (Choice (A, B, C, D))
import qualified Types.Exercise.EditQuiz as EQ
import qualified Types.Exercise.NewQuiz as NQ
import Types.Exercise.Quiz (Quiz)

allQuizzes :: Pool Connection -> Int -> Int -> IO [Quiz]
allQuizzes conns courseId lessonNumber = getMany conns allQuizzesQ (courseId, lessonNumber)

quizById :: Pool Connection -> Int -> IO (Maybe Quiz)
quizById conns exerciseId = getOne conns quizByIdQ (Only exerciseId)

insertQuiz :: Pool Connection -> Int -> Int -> NQ.NewQuiz -> IO Quiz
insertQuiz conns courseId lessonNumber newQuiz =
        insertReturning
                conns
                insertQuizQ
                ( NQ.grade newQuiz
                , NQ.title newQuiz
                , courseId
                , lessonNumber
                , NQ.question newQuiz
                , NQ.options newQuiz A
                , NQ.options newQuiz B
                , NQ.options newQuiz C
                , NQ.options newQuiz D
                , NQ.correct newQuiz
                )

updateQuiz :: Pool Connection -> Int -> EQ.EditQuiz -> IO (Maybe Quiz)
updateQuiz conns exerciseId editQuiz =
        updateReturning
                conns
                updateQuizQ
                ( EQ.question editQuiz
                , EQ.options editQuiz A
                , EQ.options editQuiz B
                , EQ.options editQuiz C
                , EQ.options editQuiz D
                , EQ.correct editQuiz
                , exerciseId
                )
