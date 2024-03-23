{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Users.Courses where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

userCoursesByIdQ :: Query
userCoursesByIdQ =
        toSqlQuery
                [ "SELECT"
                , "c.id, c.title, c.description, c.difficulty,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.joined,"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.passwordHash"
                , "FROM courses c"
                , "JOIN universities u"
                , "ON c.university = u.id"
                , "JOIN users us"
                , "ON c.instructor = us.id"
                , "JOIN enrollments e"
                , "ON e.courseId = c.id"
                , "JOIN users uss"
                , "ON uss.id = e.userId"
                , "WHERE uss.id = ?"
                ]
