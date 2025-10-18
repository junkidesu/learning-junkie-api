{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.Self.Avatar.Delete (API, handler) where

import LearningJunkie.Users.Database (removeUserAvatar, toUserType)
import LearningJunkie.Users.User
import LearningJunkie.Web.AppM (AppM)
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Delete user avatar"
        :> JWTAuth
        :> Verb 'DELETE 200 '[JSON] User

handler :: AuthResult Auth.AuthUser -> AppM User
handler (Authenticated authUser) = do
    mbUser <- removeUserAvatar (Auth.id authUser)
    case mbUser of
        Nothing -> throwError err404
        Just user -> return $ toUserType user
handler _ = throwError err401
