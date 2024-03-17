module Database.Operations.Lessons (
        allLessons,
        insertLesson,
        lessonByNumber,
) where

import Data.Pool (Pool)
import Database
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.Queries.Lessons (allLessonsQ, insertLessonQ, lessonByNumberQ)
import Types.Lesson (Lesson)
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
