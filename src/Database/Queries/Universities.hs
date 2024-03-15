{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Universities where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

allUniversitiesQ :: Query
allUniversitiesQ = "SELECT * FROM universities"

insertUniversityQ :: Query
insertUniversityQ =
        toSqlQuery
                [ "INSERT INTO universities (name, abbreviation, year)"
                , "VALUES (?, ?, ?)"
                , "RETURNING *"
                ]
