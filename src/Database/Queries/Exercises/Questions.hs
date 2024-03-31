{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Exercises.Questions where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

allQuestionsQ :: Query
allQuestionsQ =
        toSqlQuery
                [ "SELECT e.id, e.title, e.grade, q.question, q.answer,"
                , "c.id, c.title, c.description, c.difficulty, c.banner,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.logo, u.joined,"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.avatar, us.passwordHash,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.logo, u.joined,"
                , "(SELECT CASE WHEN SUM(grade) IS NULL THEN 0 ELSE SUM(grade) END"
                , "FROM courses"
                , "LEFT JOIN exercises"
                , "ON courses.id = course WHERE courses.id = c.id) as totalPoints,"
                , "(SELECT COUNT(userId)"
                , "FROM courses"
                , "LEFT JOIN enrollments"
                , "ON courses.id = courseId WHERE courses.id = c.id) as enrollmentsCount"
                , "FROM questions q"
                , "JOIN exercises e"
                , "ON q.id = e.id"
                , "JOIN courses c"
                , "ON e.course = c.id"
                , "JOIN users us"
                , "ON us.id = c.instructor"
                , "JOIN universities u"
                , "ON u.id = c.university"
                , "WHERE e.course = ? AND e.lesson = ?"
                ]

questionByIdQ :: Query
questionByIdQ =
        toSqlQuery
                [ "SELECT e.id, e.title, e.grade, q.question, q.answer,"
                , "c.id, c.title, c.description, c.difficulty, c.banner,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.logo, u.joined,"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.avatar, us.passwordHash,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.logo, u.joined,"
                , "(SELECT CASE WHEN SUM(grade) IS NULL THEN 0 ELSE SUM(grade) END"
                , "FROM courses"
                , "LEFT JOIN exercises"
                , "ON courses.id = course WHERE courses.id = c.id) as totalPoints,"
                , "(SELECT COUNT(userId)"
                , "FROM courses"
                , "LEFT JOIN enrollments"
                , "ON courses.id = courseId WHERE courses.id = c.id) as enrollmentsCount"
                , "FROM questions q"
                , "JOIN exercises e"
                , "ON q.id = e.id"
                , "JOIN courses c"
                , "ON e.course = c.id"
                , "JOIN users us"
                , "ON us.id = c.instructor"
                , "JOIN universities u"
                , "ON u.id = c.university"
                , "WHERE e.id = ?"
                ]

insertQuestionQ :: Query
insertQuestionQ =
        toSqlQuery
                [ "WITH inserted_exercise AS ("
                , "INSERT INTO exercises (grade, title, course, lesson)"
                , "VALUES (?, ?, ?, ?)"
                , "RETURNING *),"
                , "inserted_question AS ("
                , "INSERT INTO questions (id, question, answer)"
                , "VALUES ("
                , "(SELECT id FROM inserted_exercise),"
                , "?, ?)"
                , "RETURNING *)"
                , "SELECT e.id, e.title, e.grade, q.question, q.answer,"
                , "c.id, c.title, c.description, c.difficulty, c.banner,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.logo, u.joined,"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.avatar, us.passwordHash,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.logo, u.joined,"
                , "(SELECT CASE WHEN SUM(grade) IS NULL THEN 0 ELSE SUM(grade) END"
                , "FROM courses"
                , "LEFT JOIN exercises"
                , "ON courses.id = course WHERE courses.id = c.id) as totalPoints,"
                , "(SELECT COUNT(userId)"
                , "FROM courses"
                , "LEFT JOIN enrollments"
                , "ON courses.id = courseId WHERE courses.id = c.id) as enrollmentsCount"
                , "FROM inserted_exercise e"
                , "JOIN inserted_question q"
                , "ON e.id = q.id"
                , "JOIN courses c"
                , "ON e.course = c.id"
                , "JOIN users us"
                , "ON us.id = c.instructor"
                , "JOIN universities u"
                , "ON u.id = c.university"
                ]

updateQuestionQ :: Query
updateQuestionQ =
        toSqlQuery
                [ "WITH updated_question AS ("
                , "UPDATE questions"
                , "SET question = ?, answer = ?"
                , "WHERE id = ?"
                , "RETURNING *)"
                , "SELECT e.id, e.title, e.grade, q.question, q.answer,"
                , "c.id, c.title, c.description, c.difficulty, c.banner,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.logo, u.joined,"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.avatar, us.passwordHash,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.logo, u.joined,"
                , "(SELECT CASE WHEN SUM(grade) IS NULL THEN 0 ELSE SUM(grade) END"
                , "FROM courses"
                , "LEFT JOIN exercises"
                , "ON courses.id = course WHERE courses.id = c.id) as totalPoints,"
                , "(SELECT COUNT(userId)"
                , "FROM courses"
                , "LEFT JOIN enrollments"
                , "ON courses.id = courseId WHERE courses.id = c.id) as enrollmentsCount"
                , "FROM updated_question q"
                , "JOIN exercises e"
                , "ON q.id = e.id"
                , "JOIN courses c"
                , "ON e.course = c.id"
                , "JOIN users us"
                , "ON us.id = c.instructor"
                , "JOIN universities u"
                , "ON u.id = c.university"
                ]
