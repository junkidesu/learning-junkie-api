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

quizByIdQ :: Query
quizByIdQ =
        toSqlQuery
                [ "SELECT ex.id, ex.grade, q.question, q.optionA, q.optionB, q.optionC, q.optionD, q.correct"
                , "FROM quizzes q"
                , "JOIN exercises ex"
                , "ON q.id = ex.id"
                , "WHERE ex.id = ?"
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

updateQuizQ :: Query
updateQuizQ =
        toSqlQuery
                [ "WITH updated_quiz AS ("
                , "UPDATE quizzes"
                , "SET question = ?, optionA = ?, optionB = ?, optionC = ?, optionD = ?, correct = ?"
                , "WHERE id = ?"
                , "RETURNING *)"
                , "SELECT e.id, e.grade, q.question, q.optionA, q.optionB, q.optionC, q.optionD, q.correct"
                , "FROM updated_quiz q"
                , "JOIN exercises e"
                , "ON q.id = e.id"
                ]
