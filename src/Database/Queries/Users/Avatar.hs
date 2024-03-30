{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Users.Avatar where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

setAvatarQ :: Query
setAvatarQ =
        toSqlQuery
                [ "WITH updated_user AS ("
                , "UPDATE users"
                , "SET avatar = ?"
                , "WHERE id = ?"
                , "RETURNING *)"
                , "SELECT"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.avatar, us.passwordHash,"
                , "un.id, un.name, un.abbreviation, un.year, un.url, un.joined"
                , "FROM updated_user us"
                , "LEFT JOIN universities un"
                , "ON us.university = un.id"
                ]

deleteUserAvatarQ :: Query
deleteUserAvatarQ =
        toSqlQuery
                [ "WITH updated_user AS ("
                , "UPDATE users"
                , "SET avatar = NULL"
                , "WHERE id = ?"
                , "RETURNING *)"
                , "SELECT"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.avatar, us.passwordHash,"
                , "un.id, un.name, un.abbreviation, un.year, un.url, un.joined"
                , "FROM updated_user us"
                , "LEFT JOIN universities un"
                , "ON us.university = un.id"
                ]
