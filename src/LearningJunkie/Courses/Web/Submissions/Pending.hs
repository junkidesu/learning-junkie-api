{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Courses.Web.Submissions.Pending where

import Data.Int (Int32)
import LearningJunkie.Submissions.Database (selectPendingSubmissions, toSubmissionType)
import LearningJunkie.Submissions.Submission (Submission)
import LearningJunkie.Web.AppM (AppM)
import LearningJunkie.Web.Auth.Permissions (requirePermissionWithId)
import qualified LearningJunkie.Web.Auth.Permissions as Permissions
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Get all the pending submissions in a course"
        :> "submissions"
        :> JWTAuth
        :> Get '[JSON] [Submission]

handler :: Int32 -> AuthResult Auth.AuthUser -> AppM [Submission]
handler courseId (Authenticated authUser) =
    requirePermissionWithId
        authUser
        ( Permissions.Permission
            Permissions.SameInstructor
            Permissions.Update
            Permissions.Course
        )
        (Just courseId)
        $ do
            map toSubmissionType <$> selectPendingSubmissions courseId
handler _ _ = throwError err401{errBody = "Not authenticated"}
