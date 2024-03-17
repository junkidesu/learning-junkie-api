module Database.Operations.Lessons (
        allLessons,
        insertLesson,
        lessonByNumber,
        deleteLesson,
        updateLesson,
) where

import Data.Pool (Pool)
import Database
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.Queries.Lessons (allLessonsQ, deleteLessonQ, insertLessonQ, lessonByNumberQ, updateLessonQ)
import Types.Lesson (Lesson)
import qualified Types.Lesson.EditLesson as UL
import qualified Types.Lesson.NewLesson as NL

allLessons :: Pool Connection -> Int -> IO [Lesson]
allLessons conns courseId = getMany conns allLessonsQ (Only courseId)

lessonByNumber :: Pool Connection -> Int -> Int -> IO (Maybe Lesson)
lessonByNumber conns courseId lessonNumber =
        getOne
                conns
                lessonByNumberQ
                (courseId, lessonNumber)

insertLesson :: Pool Connection -> Int -> NL.NewLesson -> IO Lesson
insertLesson conns courseId newLesson =
        insertReturning
                conns
                insertLessonQ
                ( NL.number newLesson
                , NL.title newLesson
                , NL.description newLesson
                , NL.content newLesson
                , courseId
                )

deleteLesson :: Pool Connection -> Int -> Int -> IO ()
deleteLesson conns courseId lessonNumber = delete conns deleteLessonQ (courseId, lessonNumber)

updateLesson :: Pool Connection -> Int -> Int -> UL.EditLesson -> IO (Maybe Lesson)
updateLesson conns courseId lessonNumber editLesson =
        updateReturning
                conns
                updateLessonQ
                ( UL.content editLesson
                , UL.description editLesson
                , courseId
                , lessonNumber
                )
