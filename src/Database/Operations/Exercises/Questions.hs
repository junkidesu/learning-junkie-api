module Database.Operations.Exercises.Questions (allQuestions, insertQuestion) where

import Data.Pool (Pool)
import Database (getMany, insertReturning)
import Database.PostgreSQL.Simple (Connection)
import Database.Queries.Exercises.Questions (allQuestionsQ, insertQuestionQ)
import qualified Types.Exercise.NewQuestion as NQ
import Types.Exercise.Question (Question)

allQuestions :: Pool Connection -> Int -> Int -> IO [Question]
allQuestions conns courseId lessonNumber = getMany conns allQuestionsQ (courseId, lessonNumber)

insertQuestion :: Pool Connection -> Int -> Int -> NQ.NewQuestion -> IO Question
insertQuestion conns courseId lessonNumber newQuestion =
        insertReturning
                conns
                insertQuestionQ
                ( NQ.grade newQuestion
                , courseId
                , lessonNumber
                , NQ.question newQuestion
                , NQ.answer newQuestion
                )
