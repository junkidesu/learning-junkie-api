{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Courses.Enrollments where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

usersEnrolledInCourseQ :: Query
usersEnrolledInCourseQ =
        toSqlQuery
                [ "SELECT u.id, u.joined, u.name, u.birthday, u.education, u.role, u.email, u.passwordHash,"
                , "un.id, un.name, un.abbreviation, un.year, un.url, un.joined"
                , "FROM users u"
                , "LEFT JOIN universities un"
                , "ON u.university = un.id"
                , "JOIN enrollments e"
                , "ON u.id = e.userId"
                , "JOIN courses c"
                , "ON c.id = e.courseId"
                , "WHERE c.id = ?"
                ]

enrollUserInCourseQ :: Query
enrollUserInCourseQ =
        toSqlQuery
                [ "INSERT INTO enrollments (userId, courseId)"
                , "VALUES (?, ?)"
                , "ON CONFLICT DO NOTHING"
                ]

checkEnrollmentQ :: Query
checkEnrollmentQ =
        toSqlQuery
                [ "SELECT u.id, u.joined, u.name, u.birthday, u.education, u.role, u.email, u.passwordHash,"
                , "un.id, un.name, un.abbreviation, un.year, un.url, un.joined"
                , "FROM users u"
                , "LEFT JOIN universities un"
                , "ON u.university = un.id"
                , "JOIN enrollments e"
                , "ON u.id = e.userId"
                , "JOIN courses c"
                , "ON c.id = e.courseId"
                , "WHERE c.id = ? AND u.id = ?"
                ]
