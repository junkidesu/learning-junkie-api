{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.Self.Submissions.Successful where

import LearningJunkie.Submissions.Database (selectUniqueSubmissionsByUserId, toSubmissionType)
import LearningJunkie.Submissions.Submission (Submission)
import LearningJunkie.Web.AppM (AppM)
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Get all submissions by authenticated user"
        :> "successful"
        :> JWTAuth
        :> Get '[JSON] [Submission]

handler :: AuthResult Auth.AuthUser -> AppM [Submission]
handler (Authenticated authUser) =
    map toSubmissionType
        <$> selectUniqueSubmissionsByUserId (Auth.id authUser)
handler _ = throwError err401
