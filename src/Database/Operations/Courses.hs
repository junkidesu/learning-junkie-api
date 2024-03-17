module Database.Operations.Courses (
        allCourses,
        insertCourse,
        courseById,
        deleteCourse,
        universityCoursesById,
) where

import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple
import Database.Queries.Courses (
        allCoursesQ,
        courseByIdQ,
        deleteCourseQ,
        insertCourseQ,
        universityCoursesByIdQ,
 )
import Types.Course (Course)
import qualified Types.Course.NewCourse as NC

allCourses :: Pool Connection -> IO [Course]
allCourses conns =
        withResource conns $ \conn ->
                query_ conn allCoursesQ

courseById :: Pool Connection -> Int -> IO (Maybe Course)
courseById conns courseId =
        withResource conns $ \conn -> do
                found <- query conn courseByIdQ (Only courseId)

                case found of
                        [] -> pure Nothing
                        (course : _) -> pure . Just $ course

insertCourse :: Pool Connection -> Int -> NC.NewCourse -> IO (Maybe Course)
insertCourse conns universityId newCourse =
        withResource conns $ \conn -> do
                queryResult <-
                        query
                                conn
                                insertCourseQ
                                ( NC.title newCourse
                                , NC.description newCourse
                                , NC.difficulty newCourse
                                , universityId
                                , NC.instructorId newCourse
                                )

                case queryResult of
                        [] -> return Nothing
                        (course : _) -> return . pure $ course

deleteCourse :: Pool Connection -> Int -> IO ()
deleteCourse conns courseId =
        withResource conns $
                \conn -> do
                        _ <- execute conn deleteCourseQ (Only courseId)
                        return ()

universityCoursesById :: Pool Connection -> Int -> IO [Course]
universityCoursesById conns universityId =
        withResource conns $ \conn ->
                query conn universityCoursesByIdQ (Only universityId)
