{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web.Chapters.Lessons.Edit where

import Data.Int (Int32)
import LearningJunkie.Lessons.Database (toLessonType, updateLesson)
import LearningJunkie.Lessons.Lesson (Lesson)
import qualified LearningJunkie.Lessons.Lesson.Attributes as Attributes
import LearningJunkie.Web.AppM (AppM)
import LearningJunkie.Web.Auth.Permissions (requirePermissionWithId)
import qualified LearningJunkie.Web.Auth.Permissions as Permissions
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Update a lesson"
        :> JWTAuth
        :> Capture' '[Required, Description "Lesson number"] "lessonNumber" Int32
        :> ReqBody '[JSON] Attributes.Edit
        :> Put '[JSON] Lesson

handler :: Int32 -> Int32 -> AuthResult Auth.AuthUser -> Int32 -> Attributes.Edit -> AppM Lesson
handler courseId chapterNumber (Authenticated authUser) lessonNumber editLesson =
    requirePermissionWithId
        authUser
        ( Permissions.Permission
            Permissions.SameInstructor
            Permissions.Update
            Permissions.Course
        )
        (Just courseId)
        $ toLessonType
            <$> updateLesson courseId chapterNumber lessonNumber editLesson
handler _ _ _ _ _ = throwError err401
