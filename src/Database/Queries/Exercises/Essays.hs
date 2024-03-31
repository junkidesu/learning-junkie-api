{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Exercises.Essays where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

allEssaysQ :: Query
allEssaysQ =
        toSqlQuery
                [ "SELECT e.id, e.title, e.grade, es.task, es.model,"
                , "c.id, c.title, c.description, c.difficulty,"
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
                , "FROM essays es"
                , "JOIN exercises e"
                , "ON es.id = e.id"
                , "JOIN courses c"
                , "ON e.course = c.id"
                , "JOIN users us"
                , "ON us.id = c.instructor"
                , "JOIN universities u"
                , "ON u.id = c.university"
                , "WHERE e.course = ? AND e.lesson = ?"
                ]

essaysByIdQ :: Query
essaysByIdQ =
        toSqlQuery
                [ "SELECT e.id, e.title, e.grade, es.task, es.model,"
                , "c.id, c.title, c.description, c.difficulty,"
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
                , "FROM essays es"
                , "JOIN exercises e"
                , "ON es.id = e.id"
                , "JOIN courses c"
                , "ON e.course = c.id"
                , "JOIN users us"
                , "ON us.id = c.instructor"
                , "JOIN universities u"
                , "ON u.id = c.university"
                , "WHERE e.id = ?"
                ]

insertEssayQ :: Query
insertEssayQ =
        toSqlQuery
                [ "WITH inserted_exercise AS ("
                , "INSERT INTO exercises (grade, title, course, lesson)"
                , "VALUES (?, ?, ?, ?)"
                , "RETURNING *),"
                , "inserted_essay AS ("
                , "INSERT INTO essays (id, task, model)"
                , "VALUES ("
                , "(SELECT id FROM inserted_exercise),"
                , "?, ?)"
                , "RETURNING *)"
                , "SELECT e.id, e.title, e.grade, es.task, es.model,"
                , "c.id, c.title, c.description, c.difficulty,"
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
                , "FROM inserted_essay es"
                , "JOIN inserted_exercise e"
                , "ON es.id = e.id"
                , "JOIN courses c"
                , "ON e.course = c.id"
                , "JOIN users us"
                , "ON us.id = c.instructor"
                , "JOIN universities u"
                , "ON u.id = c.university"
                ]

updateEssayQ :: Query
updateEssayQ =
        toSqlQuery
                [ "WITH updated_essay AS ("
                , "UPDATE essays"
                , "SET task = ?, model = ?"
                , "WHERE id = ?"
                , "RETURNING *)"
                , "SELECT e.id, e.title, e.grade, es.task, es.model,"
                , "c.id, c.title, c.description, c.difficulty,"
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
                , "FROM updated_essay es"
                , "JOIN exercises e"
                , "ON es.id = e.id"
                , "JOIN courses c"
                , "ON e.course = c.id"
                , "JOIN users us"
                , "ON us.id = c.instructor"
                , "JOIN universities u"
                , "ON u.id = c.university"
                ]
