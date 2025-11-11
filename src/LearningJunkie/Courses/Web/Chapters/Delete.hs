{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web.Chapters.Delete where

import Data.Int (Int32)
import LearningJunkie.Chapters.Database (deleteChapter)
import LearningJunkie.Web.AppM (AppM)
import LearningJunkie.Web.Auth.Permissions (requirePermissionWithId)
import qualified LearningJunkie.Web.Auth.Permissions as Permissions
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Delete chapter from course"
        :> JWTAuth
        :> Capture' '[Required, Description "Chapter number"] "chapterNumber" Int32
        :> Verb 'DELETE 204 '[JSON] NoContent

handler :: Int32 -> AuthResult Auth.AuthUser -> Int32 -> AppM NoContent
handler courseId (Authenticated authUser) chapterNumber = requirePermissionWithId
    authUser
    ( Permissions.Permission
        Permissions.SameInstructor
        Permissions.Update
        Permissions.Course
    )
    (Just courseId)
    $ do
        deleteChapter courseId chapterNumber

        return NoContent
handler _ _ _ = throwError err401
