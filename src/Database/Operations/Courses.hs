module Database.Operations.Courses (
        allCourses,
        insertCourse,
        courseById,
        deleteCourse,
        universityCoursesById,
) where

import Data.Pool (Pool)
import qualified Data.Text as T
import Database
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
allCourses conns = getMany_ conns allCoursesQ

courseById :: Pool Connection -> Int -> IO (Maybe Course)
courseById conns courseId =
        getOne
                conns
                courseByIdQ
                (Only courseId)

insertCourse :: Pool Connection -> Int -> NC.NewCourse -> IO Course
insertCourse conns universityId newCourse =
        insertReturning
                conns
                insertCourseQ
                ( T.strip . NC.title $ newCourse
                , T.strip . NC.description $ newCourse
                , NC.difficulty newCourse
                , universityId
                , NC.instructorId newCourse
                )

deleteCourse :: Pool Connection -> Int -> IO ()
deleteCourse conns courseId =
        delete
                conns
                deleteCourseQ
                (Only courseId)

universityCoursesById :: Pool Connection -> Int -> IO [Course]
universityCoursesById conns universityId = getMany conns universityCoursesByIdQ (Only universityId)
