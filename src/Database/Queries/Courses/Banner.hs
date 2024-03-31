{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Courses.Banner where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

setBannerQ :: Query
setBannerQ =
        toSqlQuery
                [ "WITH updated_course AS (UPDATE courses"
                , "SET banner = ?"
                , "WHERE id = ?"
                , "RETURNING *)"
                , "SELECT"
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
                , "FROM updated_course c"
                , "JOIN universities u"
                , "ON c.university = u.id"
                , "JOIN users us"
                , "ON c.instructor = us.id"
                ]

deleteCourseBannerQ :: Query
deleteCourseBannerQ =
        toSqlQuery
                [ "WITH updated_course AS (UPDATE courses"
                , "SET banner = NULL"
                , "WHERE id = ?"
                , "RETURNING *)"
                , "SELECT"
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
                , "FROM updated_course c"
                , "JOIN universities u"
                , "ON c.university = u.id"
                , "JOIN users us"
                , "ON c.instructor = us.id"
                ]
