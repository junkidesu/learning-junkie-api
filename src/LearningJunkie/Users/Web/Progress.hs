{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.Progress where

import Data.Int (Int32)
import LearningJunkie.Progress.Database (selectUserProgress, toProgressType)
import LearningJunkie.Progress.Progress (Progress)
import LearningJunkie.Web.AppM (AppM)
import LearningJunkie.Web.Auth.Permissions (requireAdmin)
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "View user progress by user ID"
        :> JWTAuth
        :> "progress"
        :> Get '[JSON] [Progress]

handler :: Int32 -> AuthResult Auth.AuthUser -> AppM [Progress]
handler userId (Authenticated authUser) =
    requireAdmin authUser $
        map toProgressType
            <$> selectUserProgress userId
handler _ _ = undefined
