module Database.Operations.Courses.Enrollments (
        enrollUserInCourse,
        usersEnrolledInCourse,
        userIsEnrolled,
) where

import Data.Pool (Pool)
import Database (getMany, getOne, insert)
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.Queries.Courses.Enrollments (enrollUserInCourseQ, userIsEnrolledQ, usersEnrolledInCourseQ)
import Types.User (User)

usersEnrolledInCourse :: Pool Connection -> Int -> IO [User]
usersEnrolledInCourse conns courseId = getMany conns usersEnrolledInCourseQ (Only courseId)

enrollUserInCourse :: Pool Connection -> Int -> Int -> IO ()
enrollUserInCourse conns userId courseId = insert conns enrollUserInCourseQ (userId, courseId)

userIsEnrolled :: Pool Connection -> Int -> Int -> IO Bool
userIsEnrolled conns courseId userId = do
        mbUser <- getOne conns userIsEnrolledQ (courseId, userId) :: IO (Maybe User)

        case mbUser of
                Nothing -> return False
                Just _ -> return True
