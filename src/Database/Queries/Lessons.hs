{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Lessons where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

allLessonsQ :: Query
allLessonsQ =
        toSqlQuery
                [ "SELECT l.number, l.title, l.description, l.content"
                , "FROM lessons l"
                , "JOIN courses c"
                , "ON l.course = c.id"
                , "WHERE c.id = ?"
                ]

lessonByNumberQ :: Query
lessonByNumberQ =
        toSqlQuery
                [ "SELECT l.number, l.title, l.description, l.content"
                , "FROM lessons l"
                , "JOIN courses c"
                , "ON l.course = c.id"
                , "WHERE c.id = ? AND l.number = ?"
                ]

insertLessonQ :: Query
insertLessonQ =
        toSqlQuery
                [ "WITH inserted_lesson AS ("
                , "INSERT INTO lessons"
                , "(number, title, description, content, course)"
                , "VALUES (?, ?, ?, ?, ?)"
                , "RETURNING *)"
                , "SELECT l.number, l.title, l.description, l.content"
                , "FROM inserted_lesson l"
                , "JOIN courses c"
                , "ON c.id = l.course"
                ]

deleteLessonQ :: Query
deleteLessonQ =
        toSqlQuery
                [ "DELETE"
                , "FROM lessons"
                , "WHERE course = ? AND number = ?"
                ]

updateLessonQ :: Query
updateLessonQ =
        toSqlQuery
                [ "UPDATE lessons"
                , "SET content = ?, description = ?"
                , "WHERE course = ? AND number = ?"
                , "RETURNING number, title, description, content"
                ]
