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
                , "ON CONFLICT DO NOTHING"
                ]

userDidSolveQ :: Query
userDidSolveQ =
        toSqlQuery
                [ "SELECT u.id, u.joined, u.name, u.birthday, u.education, u.role, u.email, u.passwordHash,"
                , "un.id, un.name, un.abbreviation, un.year, un.url, un.joined"
                , "FROM solutions s"
                , "JOIN users u"
                , "ON s.userId = u.id"
                , "LEFT JOIN universities un"
                , "ON un.id = u.university"
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
