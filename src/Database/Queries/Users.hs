{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Users where

import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple (Query)
import Database.PostgreSQL.Simple.Types (Query (Query))

allUsersQ :: Query
allUsersQ = "SELECT * FROM users"

insertUserQ :: Query
insertUserQ =
        Query . BS.unlines $
                [ "INSERT INTO users"
                , "(name, birthday, education, role, email, passwordHash)"
                , "VALUES (?, ?, ?, ?, ?, ?)"
                , "RETURNING *"
                ]

userByIdQ :: Query
userByIdQ =
        Query . BS.unlines $
                [ "SELECT *"
                , "FROM users"
                , "WHERE id = ?"
                ]
