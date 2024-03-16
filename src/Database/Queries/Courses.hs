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
                , "u.id, u.name, u.abbreviation, u.year, u.joined"
                , "FROM courses c"
                , "JOIN universities u"
                , "ON c.university = u.id"
                ]

insertCourseQ :: Query
insertCourseQ =
        toSqlQuery
                [ "WITH inserted_course AS ("
                , "INSERT INTO courses (title, description, difficulty, university)"
                , "VALUES (?, ?, ?, ?)"
                , "RETURNING *)"
                , "SELECT ic.id, ic.title, ic.description, ic.difficulty,"
                , "u.id, u.name, u.abbreviation, u.year, u.joined"
                , "FROM inserted_course ic"
                , "JOIN universities u"
                , "ON ic.university = u.id"
                ]

universityCoursesQ :: Query
universityCoursesQ =
        toSqlQuery
                [ "SELECT"
                , "c.id, c.title, c.description, c.difficulty,"
                , "u.id, u.name, u.abbreviation, u.year, u.joined"
                , "FROM courses c"
                , "JOIN universities u"
                , "ON c.university = u.id"
                , "WHERE u.id = ?"
                ]
