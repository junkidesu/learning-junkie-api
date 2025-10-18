{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web.Chapters.Add where

import Data.Int (Int32)
import LearningJunkie.Chapters.Chapter (Chapter)
import qualified LearningJunkie.Chapters.Chapter.Attributes as Attributes
import LearningJunkie.Chapters.Database (insertChapter, toChapterType)
import LearningJunkie.Web.AppM (AppM)
import LearningJunkie.Web.Auth.Permissions (requirePermissionWithId)
import qualified LearningJunkie.Web.Auth.Permissions as Permissions
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Add a chapter to a course"
        :> JWTAuth
        :> ReqBody '[JSON] Attributes.New
        :> PostCreated '[JSON] Chapter

handler :: Int32 -> AuthResult Auth.AuthUser -> Attributes.New -> AppM Chapter
handler courseId (Authenticated authUser) newChapter =
    requirePermissionWithId
        authUser
        ( Permissions.Permission
            Permissions.SameInstructor
            Permissions.Update
            Permissions.Course
        )
        (Just courseId)
        $ toChapterType <$> insertChapter courseId newChapter
handler _ _ _ = throwError err401
