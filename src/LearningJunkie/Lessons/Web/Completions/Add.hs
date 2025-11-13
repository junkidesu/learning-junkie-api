{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Lessons.Web.Completions.Add where

import Data.Int (Int32)
import qualified LearningJunkie.Chapters.Chapter as Chapter
import qualified LearningJunkie.Courses.Course as Course
import LearningJunkie.Enrollments.Database (checkEnrollment)
import LearningJunkie.LessonCompletions.Database (insertLessonCompletion, toLessonCompletionType)
import LearningJunkie.LessonCompletions.LessonCompletion (LessonCompletion)
import LearningJunkie.Lessons.Database (selectLessonById, toLessonType)
import qualified LearningJunkie.Lessons.Lesson as Lesson
import LearningJunkie.Web.AppM (AppM)
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Mark a lesson complete for authenticated user by ID"
        :> JWTAuth
        :> PostCreated '[JSON] LessonCompletion

handler :: Int32 -> AuthResult Auth.AuthUser -> AppM LessonCompletion
handler lessonId (Authenticated authUser) = do
    mbLesson <-
        (toLessonType <$>)
            <$> selectLessonById lessonId

    case mbLesson of
        Nothing -> throwError err404{errBody = "Course not found"}
        Just lesson -> do
            isEnrolled <- checkEnrollment (Auth.id authUser) (Course.id . Chapter.course . Lesson.chapter $ lesson)

            if isEnrolled
                then
                    toLessonCompletionType
                        <$> insertLessonCompletion (Auth.id authUser) lessonId
                else throwError err401{errBody = "Not enrolled in the course"}
handler _ _ = throwError err401
