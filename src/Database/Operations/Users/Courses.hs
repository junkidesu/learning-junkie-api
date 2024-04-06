module Database.Operations.Users.Courses (userCoursesById, taughtCourses) where

import Data.Pool (Pool)
import Database (getMany)
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.Queries.Users.Courses (taughtCoursesQ, userCoursesByIdQ)
import Types.Course (Course)

userCoursesById :: Pool Connection -> Int -> IO [Course]
userCoursesById conns userId = getMany conns userCoursesByIdQ (Only userId)

taughtCourses :: Pool Connection -> Int -> IO [Course]
taughtCourses conns userId = getMany conns taughtCoursesQ (Only userId)
