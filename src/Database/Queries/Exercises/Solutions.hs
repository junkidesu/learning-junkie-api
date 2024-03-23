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

userDidSolveQ :: Query
userDidSolveQ =
        toSqlQuery
                [ "SELECT u.id, u.joined, u.name, u.birthday, u.education, u.role, u.email, u.passwordHash"
                , "FROM solutions s"
                , "JOIN users u"
                , "ON s.userId = u.id"
                , "WHERE exerciseId = ? AND userId = ?"
                ]

questionSolutionQ :: Query
questionSolutionQ =
        toSqlQuery
                [ "SELECT answer"
                , "FROM questions"
                , "WHERE id = ?"
                ]

essayModelSolutionQ :: Query
essayModelSolutionQ =
        toSqlQuery
                [ "SELECT model"
                , "FROM essays"
                , "WHERE id = ?"
                ]

quizSolutionQ :: Query
quizSolutionQ =
        toSqlQuery
                [ "SELECT correct"
                , "FROM quizzes"
                , "WHERE id = ?"
                ]
