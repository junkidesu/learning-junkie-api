module Database.Operations.Exercises.Questions (allQuestions, questionById, insertQuestion, updateQuestion) where

import Data.Pool (Pool)
import qualified Data.Text as T
import Database (getMany, getOne, insertReturning, updateReturning)
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.Queries.Exercises.Questions (allQuestionsQ, insertQuestionQ, questionByIdQ, updateQuestionQ)
import qualified Types.Exercise.EditQuestion as EQ
import qualified Types.Exercise.NewQuestion as NQ
import Types.Exercise.Question (Question)

allQuestions :: Pool Connection -> Int -> Int -> IO [Question]
allQuestions conns courseId lessonNumber = getMany conns allQuestionsQ (courseId, lessonNumber)

questionById :: Pool Connection -> Int -> IO (Maybe Question)
questionById conns exerciseId = getOne conns questionByIdQ (Only exerciseId)

insertQuestion :: Pool Connection -> Int -> Int -> NQ.NewQuestion -> IO Question
insertQuestion conns courseId lessonNumber newQuestion =
        insertReturning
                conns
                insertQuestionQ
                ( NQ.grade newQuestion
                , T.strip <$> NQ.title newQuestion
                , courseId
                , lessonNumber
                , NQ.question newQuestion
                , T.strip . NQ.answer $ newQuestion
                )

updateQuestion :: Pool Connection -> Int -> EQ.EditQuestion -> IO (Maybe Question)
updateQuestion conns exerciseId editQuestion =
        updateReturning
                conns
                updateQuestionQ
                ( EQ.question editQuestion
                , T.strip . EQ.answer $ editQuestion
                , exerciseId
                )
