{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web.Chapters.Lessons.Add where

import Data.Int (Int32)
import LearningJunkie.Lessons.Database (insertLesson, toLessonType)
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
    Summary "Add a lesson to a chapter in a course"
        :> JWTAuth
        :> ReqBody '[JSON] Attributes.New
        :> PostCreated '[JSON] Lesson

handler :: Int32 -> Int32 -> AuthResult Auth.AuthUser -> Attributes.New -> AppM Lesson
handler courseId chapterNumber (Authenticated authUser) newLesson =
    requirePermissionWithId
        authUser
        ( Permissions.Permission
            Permissions.SameInstructor
            Permissions.Update
            Permissions.Course
        )
        (Just courseId)
        $ toLessonType
            <$> insertLesson courseId chapterNumber newLesson
handler _ _ _ _ = throwError err401
