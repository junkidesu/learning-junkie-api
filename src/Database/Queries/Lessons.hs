{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Lessons where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

allLessonsQ :: Query
allLessonsQ =
        toSqlQuery
                [ "SELECT l.number, l.title, l.description, l.content,"
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
                , "FROM lessons l"
                , "JOIN courses c"
                , "ON l.course = c.id"
                , "JOIN universities u"
                , "ON c.university = u.id"
                , "JOIN users us"
                , "ON c.instructor = us.id"
                , "WHERE c.id = ?"
                , "ORDER BY l.number"
                ]

lessonByNumberQ :: Query
lessonByNumberQ =
        toSqlQuery
                [ "SELECT l.number, l.title, l.description, l.content,"
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
                , "FROM lessons l"
                , "JOIN courses c"
                , "ON l.course = c.id"
                , "JOIN universities u"
                , "ON c.university = u.id"
                , "JOIN users us"
                , "ON c.instructor = us.id"
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
                , "SELECT l.number, l.title, l.description, l.content,"
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
                , "FROM inserted_lesson l"
                , "JOIN courses c"
                , "ON l.course = c.id"
                , "JOIN universities u"
                , "ON c.university = u.id"
                , "JOIN users us"
                , "ON c.instructor = us.id"
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
                [ "WITH updated_lesson AS (UPDATE lessons"
                , "SET content = ?, description = ?"
                , "WHERE course = ? AND number = ?"
                , "RETURNING *)"
                , "SELECT l.number, l.title, l.description, l.content,"
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
                , "FROM updated_lesson l"
                , "JOIN courses c"
                , "ON l.course = c.id"
                , "JOIN universities u"
                , "ON c.university = u.id"
                , "JOIN users us"
                , "ON c.instructor = us.id"
                ]
