{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Universities where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

allUniversitiesQ :: Query
allUniversitiesQ = "SELECT * FROM universities"

universityByIdQ :: Query
universityByIdQ = "SELECT * FROM universities WHERE id = ?"

insertUniversityQ :: Query
insertUniversityQ =
        toSqlQuery
                [ "INSERT INTO universities (name, abbreviation, year, url)"
                , "VALUES (?, ?, ?, ?)"
                , "RETURNING *"
                ]
