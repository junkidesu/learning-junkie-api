{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Lessons.Web.Edit where

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
    Summary "Update a lesson by ID"
        :> JWTAuth
        :> ReqBody '[JSON] Attributes.Edit
        :> Put '[JSON] Lesson

handler :: Int32 -> AuthResult Auth.AuthUser -> Attributes.Edit -> AppM Lesson
handler lessonId (Authenticated authUser) editLesson =
    requirePermissionWithId
        authUser
        ( Permissions.Permission
            Permissions.SameInstructor
            Permissions.Update
            Permissions.Lesson
        )
        (Just lessonId)
        $ toLessonType
            <$> updateLesson lessonId editLesson
handler _ _ _ = throwError err401
