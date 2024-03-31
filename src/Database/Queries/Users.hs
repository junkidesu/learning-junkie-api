{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Users where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

allUsersQ :: Query
allUsersQ =
        toSqlQuery
                [ "SELECT"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.avatar, us.passwordHash,"
                , "un.id, un.name, un.abbreviation, un.year, un.url, un.logo, un.joined"
                , "FROM users us"
                , "LEFT JOIN universities un"
                , "ON us.university = un.id"
                ]

insertUserQ :: Query
insertUserQ =
        toSqlQuery
                [ "WITH inserted_user AS (INSERT INTO users"
                , "(name, birthday, education, role, email, passwordHash)"
                , "VALUES (?, ?, ?, 'student', ?, ?)"
                , "RETURNING *)"
                , "SELECT"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.passwordHash,"
                , "un.id, un.name, un.abbreviation, un.year, un.url, un.logo, un.joined"
                , "FROM inserted_user us"
                , "LEFT JOIN universities un"
                , "ON us.university = un.id"
                ]

userByIdQ :: Query
userByIdQ =
        toSqlQuery
                [ "SELECT"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.avatar, us.passwordHash,"
                , "un.id, un.name, un.abbreviation, un.year, un.url, un.logo, un.joined"
                , "FROM users us"
                , "LEFT JOIN universities un"
                , "ON us.university = un.id"
                , "WHERE us.id = ?"
                ]

userByEmailQ :: Query
userByEmailQ =
        toSqlQuery
                [ "SELECT"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.avatar, us.passwordHash,"
                , "un.id, un.name, un.abbreviation, un.year, un.url, un.logo, un.joined"
                , "FROM users us"
                , "LEFT JOIN universities un"
                , "ON us.university = un.id"
                , "WHERE us.email = ?"
                ]

deleteUserQ :: Query
deleteUserQ =
        toSqlQuery
                [ "DELETE FROM users"
                , "WHERE id = ?"
                ]
