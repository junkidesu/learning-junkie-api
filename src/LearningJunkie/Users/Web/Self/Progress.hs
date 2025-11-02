{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.Self.Progress where

import LearningJunkie.Progress.Database (selectUserProgress, toProgressType)
import LearningJunkie.Progress.Progress (Progress)
import LearningJunkie.Web.AppM (AppM)
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Get authenticated user's progress"
        :> "progress"
        :> JWTAuth
        :> Get '[JSON] [Progress]

handler :: AuthResult Auth.AuthUser -> AppM [Progress]
handler (Authenticated authUser) =
    map toProgressType
        <$> selectUserProgress (Auth.id authUser)
handler _ = throwError err401
