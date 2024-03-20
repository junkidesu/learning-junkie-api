{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Exercises.Quizzes where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

allQuizzesQ :: Query
allQuizzesQ =
        toSqlQuery
                [ "SELECT ex.id, ex.grade, q.question, q.optionA, q.optionB, q.optionC, q.optionD, q.correct"
                , "FROM quizzes q"
                , "JOIN exercises ex"
                , "ON q.id = ex.id"
                , "WHERE ex.course = ? AND ex.lesson = ?"
                ]

insertQuizQ :: Query
insertQuizQ =
        toSqlQuery
                [ "WITH inserted_exercise AS ("
                , "INSERT INTO exercises (grade, course, lesson)"
                , "VALUES (?, ?, ?)"
                , "RETURNING *),"
                , "inserted_quiz AS ("
                , "INSERT INTO quizzes (id, question, optionA, optionB, optionC, optionD, correct)"
                , "VALUES ("
                , "(SELECT id FROM inserted_exercise),"
                , "?, ?, ?, ?, ?, ?)"
                , "RETURNING *)"
                , "SELECT ex.id, ex.grade, q.question, q.optionA, q.optionB, q.optionC, q.optionD, q.correct"
                , "FROM inserted_quiz q"
                , "JOIN inserted_exercise ex"
                , "ON q.id = ex.id"
                ]
