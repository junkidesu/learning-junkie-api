{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Users.Courses where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

taughtCoursesQ :: Query
taughtCoursesQ =
        toSqlQuery
                [ "SELECT"
                , "c.id, c.title, c.description, c.difficulty, c.banner,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.logo, u.joined,"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.avatar, us.passwordHash,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.logo, u.joined,"
                , "(SELECT CASE WHEN SUM(grade) IS NULL THEN 0 ELSE SUM(grade) END"
                , "FROM courses"
                , "LEFT JOIN exercises"
                , "ON courses.id = course WHERE courses.id = c.id) as totalPoints,"
                , "(SELECT COUNT(userId)"
                , "FROM courses"
                , "LEFT JOIN enrollments"
                , "ON courses.id = courseId WHERE courses.id = c.id) as enrollmentsCount"
                , "FROM courses c"
                , "JOIN universities u"
                , "ON c.university = u.id"
                , "JOIN users us"
                , "ON c.instructor = us.id"
                , "WHERE us.id = ?"
                ]

userCoursesByIdQ :: Query
userCoursesByIdQ =
        toSqlQuery
                [ "SELECT"
                , "c.id, c.title, c.description, c.difficulty, c.banner,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.logo, u.joined,"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.avatar, us.passwordHash,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.logo, u.joined,"
                , "(SELECT CASE WHEN SUM(grade) IS NULL THEN 0 ELSE SUM(grade) END"
                , "FROM courses"
                , "LEFT JOIN exercises"
                , "ON courses.id = course WHERE courses.id = c.id) as totalPoints,"
                , "(SELECT COUNT(userId)"
                , "FROM courses"
                , "LEFT JOIN enrollments"
                , "ON courses.id = courseId WHERE courses.id = c.id) as enrollmentsCount"
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
