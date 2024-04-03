{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Courses.Completions where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

courseCompletionQ :: Query
courseCompletionQ =
        toSqlQuery
                [ "SELECT"
                , "c.id, c.title, c.description, c.difficulty, c.banner,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.logo, u.joined,"
                , "ins.id, ins.joined, ins.name, ins.birthday, ins.education, ins.role, ins.email, ins.avatar, ins.passwordHash,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.logo, u.joined,"
                , "(SELECT CASE WHEN SUM(grade) IS NULL THEN 0 ELSE SUM(grade) END"
                , "FROM courses"
                , "LEFT JOIN exercises"
                , "ON courses.id = course WHERE courses.id = c.id) AS totalPoints,"
                , "(SELECT COUNT(userId)"
                , "FROM courses"
                , "LEFT JOIN enrollments"
                , "ON courses.id = courseId WHERE courses.id = c.id) AS enrollmentsCount,"
                , "st.id, st.joined, st.name, st.birthday, st.education, st.role, st.email, st.avatar, st.passwordHash,"
                , "stu.id, stu.name, stu.abbreviation, stu.year, stu.url, stu.logo, stu.joined,"
                , "com.time"
                , "FROM users st"
                , "LEFT JOIN universities stu"
                , "ON st.university = stu.id"
                , "JOIN completions com"
                , "ON com.userId = st.id"
                , "JOIN courses c"
                , "ON com.courseId = c.id"
                , "JOIN universities u"
                , "ON c.university = u.id"
                , "JOIN users ins"
                , "ON ins.id = c.instructor"
                , "WHERE c.id = ? AND st.id = ?"
                ]

insertCompletionQ :: Query
insertCompletionQ =
        toSqlQuery
                [ "INSERT INTO completions (courseId, userId)"
                , "VALUES (?, ?)"
                , "ON CONFLICT DO NOTHING"
                ]
