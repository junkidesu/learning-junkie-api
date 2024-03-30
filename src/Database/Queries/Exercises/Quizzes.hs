{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Exercises.Quizzes where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

allQuizzesQ :: Query
allQuizzesQ =
        toSqlQuery
                [ "SELECT ex.id, ex.title, ex.grade, q.question, q.optionA, q.optionB, q.optionC, q.optionD, q.correct,"
                , "c.id, c.title, c.description, c.difficulty,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.joined,"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.avatar, us.passwordHash,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.joined,"
                , "(SELECT CASE WHEN SUM(grade) IS NULL THEN 0 ELSE SUM(grade) END"
                , "FROM courses"
                , "LEFT JOIN exercises"
                , "ON courses.id = course WHERE courses.id = c.id) as totalPoints,"
                , "(SELECT COUNT(userId)"
                , "FROM courses"
                , "LEFT JOIN enrollments"
                , "ON courses.id = courseId WHERE courses.id = c.id) as enrollmentsCount"
                , "FROM quizzes q"
                , "JOIN exercises ex"
                , "ON q.id = ex.id"
                , "JOIN courses c"
                , "ON ex.course = c.id"
                , "JOIN users us"
                , "ON us.id = c.instructor"
                , "JOIN universities u"
                , "ON u.id = c.university"
                , "WHERE ex.course = ? AND ex.lesson = ?"
                ]

quizByIdQ :: Query
quizByIdQ =
        toSqlQuery
                [ "SELECT ex.id, ex.title, ex.grade, q.question, q.optionA, q.optionB, q.optionC, q.optionD, q.correct,"
                , "c.id, c.title, c.description, c.difficulty,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.joined,"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.avatar, us.passwordHash,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.joined,"
                , "(SELECT CASE WHEN SUM(grade) IS NULL THEN 0 ELSE SUM(grade) END"
                , "FROM courses"
                , "LEFT JOIN exercises"
                , "ON courses.id = course WHERE courses.id = c.id) as totalPoints,"
                , "(SELECT COUNT(userId)"
                , "FROM courses"
                , "LEFT JOIN enrollments"
                , "ON courses.id = courseId WHERE courses.id = c.id) as enrollmentsCount"
                , "FROM quizzes q"
                , "JOIN exercises ex"
                , "ON q.id = ex.id"
                , "JOIN courses c"
                , "ON ex.course = c.id"
                , "JOIN users us"
                , "ON us.id = c.instructor"
                , "JOIN universities u"
                , "ON u.id = c.university"
                , "WHERE ex.id = ?"
                ]

insertQuizQ :: Query
insertQuizQ =
        toSqlQuery
                [ "WITH inserted_exercise AS ("
                , "INSERT INTO exercises (grade, title, course, lesson)"
                , "VALUES (?, ?, ?, ?)"
                , "RETURNING *),"
                , "inserted_quiz AS ("
                , "INSERT INTO quizzes (id, question, optionA, optionB, optionC, optionD, correct)"
                , "VALUES ("
                , "(SELECT id FROM inserted_exercise),"
                , "?, ?, ?, ?, ?, ?)"
                , "RETURNING *)"
                , "SELECT ex.id, ex.title, ex.grade, q.question, q.optionA, q.optionB, q.optionC, q.optionD, q.correct,"
                , "c.id, c.title, c.description, c.difficulty,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.joined,"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.avatar, us.passwordHash,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.joined,"
                , "(SELECT CASE WHEN SUM(grade) IS NULL THEN 0 ELSE SUM(grade) END"
                , "FROM courses"
                , "LEFT JOIN exercises"
                , "ON courses.id = course WHERE courses.id = c.id) as totalPoints,"
                , "(SELECT COUNT(userId)"
                , "FROM courses"
                , "LEFT JOIN enrollments"
                , "ON courses.id = courseId WHERE courses.id = c.id) as enrollmentsCount"
                , "FROM inserted_quiz q"
                , "JOIN inserted_exercise ex"
                , "ON q.id = ex.id"
                , "JOIN courses c"
                , "ON ex.course = c.id"
                , "JOIN users us"
                , "ON us.id = c.instructor"
                , "JOIN universities u"
                , "ON u.id = c.university"
                ]

updateQuizQ :: Query
updateQuizQ =
        toSqlQuery
                [ "WITH updated_quiz AS ("
                , "UPDATE quizzes"
                , "SET question = ?, optionA = ?, optionB = ?, optionC = ?, optionD = ?, correct = ?"
                , "WHERE id = ?"
                , "RETURNING *)"
                , "SELECT ex.id, ex.title, ex.grade, q.question, q.optionA, q.optionB, q.optionC, q.optionD, q.correct,"
                , "c.id, c.title, c.description, c.difficulty,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.joined,"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.avatar, us.passwordHash,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.joined,"
                , "(SELECT CASE WHEN SUM(grade) IS NULL THEN 0 ELSE SUM(grade) END"
                , "FROM courses"
                , "LEFT JOIN exercises"
                , "ON courses.id = course WHERE courses.id = c.id) as totalPoints,"
                , "(SELECT COUNT(userId)"
                , "FROM courses"
                , "LEFT JOIN enrollments"
                , "ON courses.id = courseId WHERE courses.id = c.id) as enrollmentsCount"
                , "FROM updated_quiz q"
                , "JOIN exercises ex"
                , "ON q.id = ex.id"
                , "JOIN courses c"
                , "ON ex.course = c.id"
                , "JOIN users us"
                , "ON us.id = c.instructor"
                , "JOIN universities u"
                , "ON u.id = c.university"
                ]
