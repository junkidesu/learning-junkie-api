module Database.Operations.Courses.Enrollments (
        enrollUserInCourse,
        usersEnrolledInCourse,
        checkEnrollment,
) where

import Data.Pool (Pool)
import Database (getMany, getOne, insert)
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.Queries.Courses.Enrollments (checkEnrollmentQ, enrollUserInCourseQ, usersEnrolledInCourseQ)
import Types.User (User)

usersEnrolledInCourse :: Pool Connection -> Int -> IO [User]
usersEnrolledInCourse conns courseId = getMany conns usersEnrolledInCourseQ (Only courseId)

enrollUserInCourse :: Pool Connection -> Int -> Int -> IO ()
enrollUserInCourse conns userId courseId = insert conns enrollUserInCourseQ (userId, courseId)

checkEnrollment :: Pool Connection -> Int -> Int -> IO Bool
checkEnrollment conns courseId userId = do
        mbUser <- getOne conns checkEnrollmentQ (courseId, userId) :: IO (Maybe User)

        case mbUser of
                Nothing -> return False
                Just _ -> return True
