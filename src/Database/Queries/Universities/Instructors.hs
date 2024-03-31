{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Universities.Instructors where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

allInstructorsQ :: Query
allInstructorsQ =
        toSqlQuery
                [ "SELECT"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.avatar, us.passwordHash,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.logo, u.joined"
                , "FROM users us"
                , "JOIN universities u"
                , "ON u.id = us.university"
                , "WHERE u.id = ?"
                ]

insertInstructorQ :: Query
insertInstructorQ =
        toSqlQuery
                [ "WITH inserted_user AS ("
                , "INSERT INTO users"
                , "(name, birthday, education, role, email, passwordHash, university)"
                , "VALUES (?, ?, ?, 'instructor', ?, ?, ?)"
                , "RETURNING *)"
                , "SELECT"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.avatar, us.passwordHash,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.logo, u.joined"
                , "FROM inserted_user us"
                , "JOIN universities u"
                , "ON us.university = u.id"
                , "WHERE u.id = us.university"
                ]
