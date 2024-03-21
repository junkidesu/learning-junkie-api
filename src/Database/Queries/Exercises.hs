{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Exercises where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

exerciseByIdQ :: Query
exerciseByIdQ =
        toSqlQuery
                [ "SELECT e.id, e.grade,"
                , "c.id, c.title, c.description, c.difficulty,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.joined,"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.passwordHash"
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
