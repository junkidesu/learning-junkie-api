module Database.Operations.Courses.Enrollments (
        enrollUserInCourse,
        usersEnrolledInCourse,
) where

import Data.Pool (Pool, withResource)
import Database.Operations.Courses (courseById)
import Database.PostgreSQL.Simple (Connection, Only (Only), execute, query)
import Database.Queries.Courses.Enrollments (enrollUserInCourseQ, usersEnrolledInCourseQ)
import Types.User (User)

usersEnrolledInCourse :: Pool Connection -> Int -> IO [User]
usersEnrolledInCourse conns courseId =
        withResource conns $
                \conn -> query conn usersEnrolledInCourseQ (Only courseId)

enrollUserInCourse :: Pool Connection -> Int -> Int -> IO (Either String String)
enrollUserInCourse conns userId courseId =
        withResource conns $
                \conn -> do
                        mbCourse <- courseById conns courseId

                        case mbCourse of
                                Nothing -> return (Left "Course not found")
                                Just _ -> do
                                        _ <- execute conn enrollUserInCourseQ (userId, courseId)
                                        return (Right "Successfully enrolled!")
