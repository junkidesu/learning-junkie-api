module Database.Operations.Lessons (
        allLessons,
        insertLesson,
        lessonByNumber,
) where

import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection, Only (Only), query)
import Database.Queries.Lessons (allLessonsQ, insertLessonQ, lessonByNumberQ)
import Types.Lesson (Lesson)
import qualified Types.Lesson.NewLesson as NL

allLessons :: Pool Connection -> Int -> IO [Lesson]
allLessons conns courseId =
        withResource conns $
                \conn -> query conn allLessonsQ (Only courseId)

lessonByNumber :: Pool Connection -> Int -> Int -> IO (Maybe Lesson)
lessonByNumber conns courseId lessonNumber =
        withResource conns $
                \conn -> do
                        queryResult <- query conn lessonByNumberQ (courseId, lessonNumber)

                        case queryResult of
                                [] -> return Nothing
                                (lesson : _) -> return . pure $ lesson

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
