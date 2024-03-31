{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Users.Solutions where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

userSolutionsQ :: Query
userSolutionsQ =
        toSqlQuery
                [ "SELECT e.id, e.title, e.grade,"
                , "c.id, c.title, c.description, c.difficulty, c.banner,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.logo, u.joined,"
                , "ins.id, ins.joined, ins.name, ins.birthday, ins.education, ins.role, ins.email, us.avatar, ins.passwordHash,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.logo, u.joined,"
                , "(SELECT CASE WHEN SUM(grade) IS NULL THEN 0 ELSE SUM(grade) END"
                , "FROM courses"
                , "LEFT JOIN exercises"
                , "ON courses.id = course WHERE courses.id = c.id) as exercisesCount,"
                , "(SELECT COUNT(userId)"
                , "FROM courses"
                , "LEFT JOIN enrollments"
                , "ON courses.id = courseId WHERE courses.id = c.id) as enrollmentsCount"
                , "FROM users us"
                , "JOIN solutions s"
                , "ON us.id = s.userId"
                , "JOIN exercises e"
                , "ON e.id = s.exerciseId"
                , "JOIN courses c"
                , "ON c.id = e.course"
                , "JOIN universities u"
                , "ON u.id = c.university"
                , "JOIN users ins"
                , "ON c.instructor = ins.id"
                , "WHERE us.id = ?"
                ]
