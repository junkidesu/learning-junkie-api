{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Universities.Logo where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

setLogoQ :: Query
setLogoQ =
        toSqlQuery
                [ "UPDATE universities"
                , "SET logo = ?"
                , "WHERE id = ?"
                , "RETURNING *"
                ]

deleteUniversityLogoQ :: Query
deleteUniversityLogoQ =
        toSqlQuery
                [ "UPDATE universities"
                , "SET logo = NULL"
                , "WHERE id = ?"
                , "RETURNING *"
                ]
