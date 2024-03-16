module Database.Operations.Courses (
        allCourses,
        insertCourse,
        universityCourses,
) where

import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple
import Database.Queries.Courses (allCoursesQ, insertCourseQ, universityCoursesQ)
import Types.Course (Course)
import qualified Types.Course.NewCourse as NC

allCourses :: Pool Connection -> IO [Course]
allCourses conns =
        withResource conns $ \conn ->
                query_ conn allCoursesQ

insertCourse :: Pool Connection -> Int -> NC.NewCourse -> IO Course
insertCourse conns universityId newCourse =
        withResource conns $ \conn -> do
                [course] <-
                        query
                                conn
                                insertCourseQ
                                ( NC.title newCourse
                                , NC.description newCourse
                                , NC.difficulty newCourse
                                , universityId
                                )
                return course

universityCourses :: Pool Connection -> Int -> IO [Course]
universityCourses conns universityId =
        withResource conns $ \conn ->
                query conn universityCoursesQ (Only universityId)
