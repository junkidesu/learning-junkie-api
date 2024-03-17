module Database.Operations.Lessons (
        allLessons,
        insertLesson,
) where

import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection, Only (Only), query)
import Database.Queries.Lessons (allLessonsQ, insertLessonQ)
import Types.Lesson (Lesson)
import qualified Types.Lesson.NewLesson as NL

allLessons :: Pool Connection -> Int -> IO [Lesson]
allLessons conns courseId =
        withResource conns $
                \conn -> query conn allLessonsQ (Only courseId)

insertLesson :: Pool Connection -> Int -> NL.NewLesson -> IO Lesson
insertLesson conns courseId newLesson =
        withResource conns $
                \conn -> do
                        [lesson] <-
                                query
                                        conn
                                        insertLessonQ
                                        ( NL.number newLesson
                                        , NL.title newLesson
                                        , NL.description newLesson
                                        , NL.content newLesson
                                        , courseId
                                        )
                        return lesson
