{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.Self.Submissions.All where

import LearningJunkie.Submissions.Database (selectSubmissionsByUserId, toSubmissionType)
import LearningJunkie.Submissions.Submission (Submission)
import LearningJunkie.Web.AppM (AppM)
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Get all submissions by authenticated user"
        :> JWTAuth
        :> Get '[JSON] [Submission]

handler :: AuthResult Auth.AuthUser -> AppM [Submission]
handler (Authenticated authUser) =
    map toSubmissionType
        <$> selectSubmissionsByUserId (Auth.id authUser)
handler _ = throwError err401
