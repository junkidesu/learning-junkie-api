{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Exercises where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

exerciseByIdQ :: Query
exerciseByIdQ =
        toSqlQuery
                [ "SELECT e.id, e.title, e.grade,"
                , "c.id, c.title, c.description, c.difficulty,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.joined,"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.avatar, us.passwordHash,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.joined,"
                , "(SELECT CASE WHEN SUM(grade) IS NULL THEN 0 ELSE SUM(grade) END"
                , "FROM courses"
                , "LEFT JOIN exercises"
                , "ON courses.id = course WHERE courses.id = c.id) as totalPoints,"
                , "(SELECT COUNT(userId)"
                , "FROM courses"
                , "LEFT JOIN enrollments"
                , "ON courses.id = courseId WHERE courses.id = c.id) as enrollmentsCount"
                , "FROM exercises e"
                , "JOIN courses c"
                , "ON e.course = c.id"
                , "JOIN users us"
                , "ON us.id = c.instructor"
                , "JOIN universities u"
                , "ON u.id = c.university"
                , "WHERE e.id = ?"
                ]
deleteExerciseQ :: Query
deleteExerciseQ =
        toSqlQuery
                [ "DELETE FROM exercises"
                , "WHERE id = ?"
                ]
