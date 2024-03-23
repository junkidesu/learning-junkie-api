{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Courses where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

allCoursesQ :: Query
allCoursesQ =
        toSqlQuery
                [ "SELECT"
                , "c.id, c.title, c.description, c.difficulty,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.joined,"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.passwordHash,"
                , "(SELECT CASE WHEN SUM(grade) IS NULL THEN 0 ELSE SUM(grade) END"
                , "FROM courses"
                , "LEFT JOIN exercises"
                , "ON courses.id = course WHERE courses.id = c.id) as totalPoints,"
                , "(SELECT COUNT(userId)"
                , "FROM courses"
                , "LEFT JOIN enrollments"
                , "ON courses.id = courseId WHERE courses.id = c.id) as enrollmentsCount"
                , "FROM courses c"
                , "JOIN universities u"
                , "ON c.university = u.id"
                , "JOIN users us"
                , "ON c.instructor = us.id"
                ]

courseByIdQ :: Query
courseByIdQ =
        toSqlQuery
                [ "SELECT"
                , "c.id, c.title, c.description, c.difficulty,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.joined,"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.passwordHash,"
                , "(SELECT CASE WHEN SUM(grade) IS NULL THEN 0 ELSE SUM(grade) END"
                , "FROM courses"
                , "LEFT JOIN exercises"
                , "ON courses.id = course WHERE courses.id = c.id) as totalPoints,"
                , "(SELECT COUNT(userId)"
                , "FROM courses"
                , "LEFT JOIN enrollments"
                , "ON courses.id = courseId WHERE courses.id = c.id) as enrollmentsCount"
                , "FROM courses c"
                , "JOIN universities u"
                , "ON c.university = u.id"
                , "JOIN users us"
                , "ON c.instructor = us.id"
                , "WHERE c.id = ?"
                ]

insertCourseQ :: Query
insertCourseQ =
        toSqlQuery
                [ "WITH inserted_course AS ("
                , "INSERT INTO courses (title, description, difficulty, university, instructor)"
                , "VALUES (?, ?, ?, ?, ?)"
                , "RETURNING *)"
                , "SELECT ic.id, ic.title, ic.description, ic.difficulty,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.joined,"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.passwordHash,"
                , "(SELECT CASE WHEN SUM(grade) IS NULL THEN 0 ELSE SUM(grade) END"
                , "FROM courses"
                , "LEFT JOIN exercises"
                , "ON courses.id = course WHERE courses.id = ic.id) as totalPoints,"
                , "(SELECT COUNT(userId)"
                , "FROM courses"
                , "LEFT JOIN enrollments"
                , "ON courses.id = courseId WHERE courses.id = ic.id) as enrollmentsCount"
                , "FROM inserted_course ic"
                , "JOIN universities u"
                , "ON ic.university = u.id"
                , "JOIN users us"
                , "ON ic.instructor = us.id"
                ]

deleteCourseQ :: Query
deleteCourseQ =
        toSqlQuery
                [ "DELETE FROM courses"
                , "WHERE id = ?"
                ]

universityCoursesByIdQ :: Query
universityCoursesByIdQ =
        toSqlQuery
                [ "SELECT"
                , "c.id, c.title, c.description, c.difficulty,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.joined,"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.passwordHash,"
                , "(SELECT CASE WHEN SUM(grade) IS NULL THEN 0 ELSE SUM(grade) END"
                , "FROM courses"
                , "LEFT JOIN exercises"
                , "ON courses.id = course WHERE courses.id = c.id) as totalPoints,"
                , "(SELECT COUNT(userId)"
                , "FROM courses"
                , "LEFT JOIN enrollments"
                , "ON courses.id = courseId WHERE courses.id = c.id) as enrollmentsCount"
                , "FROM courses c"
                , "JOIN universities u"
                , "ON c.university = u.id"
                , "JOIN users us"
                , "ON c.instructor = us.id"
                , "WHERE u.id = ?"
                ]
