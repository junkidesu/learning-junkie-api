{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Courses where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

allCoursesQ :: Query
allCoursesQ =
        toSqlQuery
                [ "SELECT"
                , "c.id, c.title, c.description, c.difficulty,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.joined,"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.passwordHash,"
                , "COUNT(e.id) totalExercises"
                , "FROM courses c"
                , "JOIN universities u"
                , "ON c.university = u.id"
                , "JOIN users us"
                , "ON c.instructor = us.id"
                , "LEFT JOIN exercises e"
                , "ON e.course = c.id"
                , "GROUP BY us.id, u.id, c.id"
                ]

courseByIdQ :: Query
courseByIdQ =
        toSqlQuery
                [ "SELECT"
                , "c.id, c.title, c.description, c.difficulty,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.joined,"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.passwordHash,"
                , "COUNT(e.id) as totalExercises"
                , "FROM courses c"
                , "JOIN universities u"
                , "ON c.university = u.id"
                , "JOIN users us"
                , "ON c.instructor = us.id"
                , "LEFT JOIN exercises e"
                , "ON e.course = c.id"
                , "GROUP BY c.id, u.id, us.id"
                , "HAVING c.id = ?"
                ]

insertCourseQ :: Query
insertCourseQ =
        toSqlQuery
                [ "WITH inserted_course AS ("
                , "INSERT INTO courses (title, description, difficulty, university, instructor)"
                , "VALUES (?, ?, ?, ?, ?)"
                , "RETURNING *)"
                , "SELECT ic.id, ic.title, ic.description, ic.difficulty,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.joined,"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.passwordHash,"
                , "0 as totalExercises"
                , "FROM inserted_course ic"
                , "JOIN universities u"
                , "ON ic.university = u.id"
                , "JOIN users us"
                , "ON ic.instructor = us.id"
                ]

deleteCourseQ :: Query
deleteCourseQ =
        toSqlQuery
                [ "DELETE FROM courses"
                , "WHERE id = ?"
                ]

universityCoursesByIdQ :: Query
universityCoursesByIdQ =
        toSqlQuery
                [ "SELECT"
                , "c.id, c.title, c.description, c.difficulty,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.joined,"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.passwordHash,"
                , "COUNT(e.id) as totalExercises"
                , "FROM courses c"
                , "JOIN universities u"
                , "ON c.university = u.id"
                , "JOIN users us"
                , "ON c.instructor = us.id"
                , "LEFT JOIN exercises e"
                , "ON e.course = c.id"
                , "GROUP BY c.id, u.id, us.id"
                , "HAVING u.id = ?"
                ]
