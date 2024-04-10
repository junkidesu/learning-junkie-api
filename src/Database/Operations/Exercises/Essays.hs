module Database.Operations.Exercises.Essays (allEssays, essayById, insertEssay, updateEssay) where

import Data.Pool (Pool)
import qualified Data.Text as T
import Database (getMany, getOne, insertReturning, updateReturning)
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.Queries.Exercises.Essays (allEssaysQ, essaysByIdQ, insertEssayQ, updateEssayQ)
import qualified Types.Exercise.EditEssay as EE
import Types.Exercise.Essay (Essay)
import qualified Types.Exercise.NewEssay as NE

allEssays :: Pool Connection -> Int -> Int -> IO [Essay]
allEssays conns courseId lessonNumber = getMany conns allEssaysQ (courseId, lessonNumber)

essayById :: Pool Connection -> Int -> IO (Maybe Essay)
essayById conns exerciseId = getOne conns essaysByIdQ (Only exerciseId)

insertEssay :: Pool Connection -> Int -> Int -> NE.NewEssay -> IO Essay
insertEssay conns courseId lessonNumber newEssay =
        insertReturning
                conns
                insertEssayQ
                ( NE.grade newEssay
                , T.strip <$> NE.title newEssay
                , courseId
                , lessonNumber
                , NE.task newEssay
                , NE.model newEssay
                )

updateEssay :: Pool Connection -> Int -> EE.EditEssay -> IO (Maybe Essay)
updateEssay conns exerciseId editEssay =
        updateReturning
                conns
                updateEssayQ
                ( EE.task editEssay
                , EE.model editEssay
                , exerciseId
                )
