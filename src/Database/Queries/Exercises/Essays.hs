{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Exercises.Essays where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

allEssaysQ :: Query
allEssaysQ =
        toSqlQuery
                [ "SELECT ex.id, ex.grade, es.task, es.model"
                , "FROM essays es"
                , "JOIN exercises ex"
                , "ON es.id = ex.id"
                , "WHERE ex.course = ? AND ex.lesson = ?"
                ]

essaysByIdQ :: Query
essaysByIdQ =
        toSqlQuery
                [ "SELECT ex.id, ex.grade, es.task, es.model"
                , "FROM essays es"
                , "JOIN exercises ex"
                , "ON es.id = ex.id"
                , "WHERE ex.id = ?"
                ]

insertEssayQ :: Query
insertEssayQ =
        toSqlQuery
                [ "WITH inserted_exercise AS ("
                , "INSERT INTO exercises (grade, course, lesson)"
                , "VALUES (?, ?, ?)"
                , "RETURNING *),"
                , "inserted_essay AS ("
                , "INSERT INTO essays (id, task, model)"
                , "VALUES ("
                , "(SELECT id FROM inserted_exercise),"
                , "?, ?)"
                , "RETURNING *)"
                , "SELECT ex.id, ex.grade, es.task, es.model"
                , "FROM inserted_essay es"
                , "JOIN inserted_exercise ex"
                , "ON es.id = ex.id"
                ]

updateEssayQ :: Query
updateEssayQ =
        toSqlQuery
                [ "WITH updated_essay AS ("
                , "UPDATE essays"
                , "SET task = ?, model = ?"
                , "WHERE id = ?"
                , "RETURNING *)"
                , "SELECT ex.id, ex.grade, es.task, es.model"
                , "FROM updated_essay es"
                , "JOIN exercises ex"
                , "ON es.id = ex.id"
                ]
