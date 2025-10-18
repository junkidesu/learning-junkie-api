{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.Submissions.All where

import Data.Int (Int32)
import LearningJunkie.Submissions.Database (selectSubmissionsByUserId, toSubmissionType)
import LearningJunkie.Submissions.Submission (Submission)
import LearningJunkie.Web.AppM (AppM)
import LearningJunkie.Web.Auth.Permissions (requireAdmin)
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Get all submissions of a user by ID"
        :> JWTAuth
        :> Get '[JSON] [Submission]

handler :: Int32 -> AuthResult Auth.AuthUser -> AppM [Submission]
handler userId (Authenticated authUser) =
    requireAdmin authUser $
        map toSubmissionType <$> selectSubmissionsByUserId userId
handler _ _ = throwError err401
