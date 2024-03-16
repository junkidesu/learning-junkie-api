module Database.Operations.Courses.Enrollments (
        enrollUserInCourse,
        usersEnrolledInCourse,
) where

import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection, Only (Only), execute, query)
import Database.Queries.Courses.Enrollments (enrollUserInCourseQ, usersEnrolledInCourseQ)
import Types.User (User)

usersEnrolledInCourse :: Pool Connection -> Int -> IO [User]
usersEnrolledInCourse conns courseId =
        withResource conns $
                \conn -> query conn usersEnrolledInCourseQ (Only courseId)

enrollUserInCourse :: Pool Connection -> Int -> Int -> IO ()
enrollUserInCourse conns userId courseId =
        withResource conns $
                \conn -> do
                        _ <- execute conn enrollUserInCourseQ (userId, courseId)
                        return ()
