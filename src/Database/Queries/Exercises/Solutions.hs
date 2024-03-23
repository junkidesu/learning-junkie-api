{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Exercises.Solutions where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

insertSolutionQ :: Query
insertSolutionQ =
        toSqlQuery
                [ "INSERT INTO solutions"
                , "(userId, exerciseId, grade)"
                , "VALUES (?, ?, ?)"
                , "RETURNING grade"
                ]
