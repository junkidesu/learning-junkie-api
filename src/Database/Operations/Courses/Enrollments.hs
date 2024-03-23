module Database.Operations.Courses.Enrollments (
        enrollUserInCourse,
        usersEnrolledInCourse,
        userIsEnrolled,
) where

import Data.Pool (Pool)
import Database (getMany, getOne, insert)
import Database.Operations.Courses (courseById)
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.Queries.Courses.Enrollments (enrollUserInCourseQ, userIsEnrolledQ, usersEnrolledInCourseQ)
import Types.User (User)

usersEnrolledInCourse :: Pool Connection -> Int -> IO [User]
usersEnrolledInCourse conns courseId = getMany conns usersEnrolledInCourseQ (Only courseId)

enrollUserInCourse :: Pool Connection -> Int -> Int -> IO (Either String String)
enrollUserInCourse conns userId courseId = do
        mbCourse <- courseById conns courseId

        case mbCourse of
                Nothing -> return (Left "Course not found")
                Just _ -> do
                        insert conns enrollUserInCourseQ (userId, courseId)
                        return (Right "Successfully enrolled!")

userIsEnrolled :: Pool Connection -> Int -> Int -> IO Bool
userIsEnrolled conns courseId userId = do
        mbUser <- getOne conns userIsEnrolledQ (courseId, userId) :: IO (Maybe User)

        case mbUser of
                Nothing -> return False
                Just _ -> return True
