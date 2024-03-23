{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Database.Queries.Users.Progress where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

userProgressQ :: Query
userProgressQ =
        toSqlQuery
                [ "SELECT"
                , "c.id, c.title, c.description, c.difficulty,"
                , "u.id, u.name, u.abbreviation, u.year, u.url, u.joined,"
                , "us.id, us.joined, us.name, us.birthday, us.education, us.role, us.email, us.passwordHash,"
                , "(SELECT COUNT(exercises.id)"
                , "FROM courses"
                , "LEFT JOIN exercises"
                , "ON courses.id = course WHERE courses.id = c.id) as exercisesCount,"
                , "(SELECT COUNT(userId)"
                , "FROM courses"
                , "LEFT JOIN enrollments"
                , "ON courses.id = courseId WHERE courses.id = c.id) as enrollmentsCount,"
                , "(SELECT COUNT(solutions.exerciseId)"
                , "FROM solutions"
                , "JOIN exercises"
                , "ON exerciseId = exercises.id"
                , "JOIN courses"
                , "ON courses.id = exercises.course WHERE courses.id = c.id AND userId = st.id) as solvedExercises"
                , "FROM users st"
                , "JOIN enrollments en"
                , "ON en.userId = st.id"
                , "LEFT JOIN courses c"
                , "ON en.courseId = c.id"
                , "LEFT JOIN universities u"
                , "ON u.id = c.university"
                , "LEFT JOIN users us"
                , "ON us.id = c.instructor"
                , "WHERE st.id = ?"
                ]
