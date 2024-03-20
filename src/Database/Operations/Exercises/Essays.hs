module Database.Operations.Exercises.Essays (allEssays, insertEssay) where

import Data.Pool (Pool)
import Database (getMany, insertReturning)
import Database.PostgreSQL.Simple (Connection)
import Database.Queries.Exercises.Essays (allEssaysQ, insertEssayQ)
import Types.Exercise.Essay (Essay)
import qualified Types.Exercise.NewEssay as NE

allEssays :: Pool Connection -> Int -> Int -> IO [Essay]
allEssays conns courseId lessonNumber = getMany conns allEssaysQ (courseId, lessonNumber)

insertEssay :: Pool Connection -> Int -> Int -> NE.NewEssay -> IO Essay
insertEssay conns courseId lessonNumber newEssay =
        insertReturning
                conns
                insertEssayQ
                ( NE.grade newEssay
                , courseId
                , lessonNumber
                , NE.task newEssay
                , NE.model newEssay
                )
