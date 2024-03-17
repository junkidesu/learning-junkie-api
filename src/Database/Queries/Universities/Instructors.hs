{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Universities.Instructors where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

allInstructorsQ :: Query
allInstructorsQ =
        toSqlQuery
                [ "SELECT"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.joined"
                , "FROM instructors i"
                , "JOIN users us"
                , "ON i.id = us.id"
                , "JOIN universities u"
                , "ON i.university = u.id"
                , "WHERE u.id = ?"
                ]

insertInstructorQ :: Query
insertInstructorQ =
        toSqlQuery
                [ "WITH inserted_user AS ("
                , "INSERT INTO users"
                , "(name, birthday, education, role, email, passwordHash)"
                , "VALUES (?, ?, ?, 'instructor', ?, ?)"
                , "RETURNING *), inserted_instructor AS ("
                , "INSERT INTO instructors"
                , "(id, university)"
                , "VALUES ((SELECT id FROM inserted_user), ?)"
                , "RETURNING *)"
                , "SELECT"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.joined"
                , "FROM inserted_user us"
                , "JOIN inserted_instructor i"
                , "ON i.id = us.id"
                , "JOIN universities u"
                , "ON i.university = u.id"
                , "WHERE u.id = i.university"
                ]
