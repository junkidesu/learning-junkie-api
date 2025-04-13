{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.Create (API, handler) where

import LearningJunkie.Users.Database (insertUser, toUserType)
import LearningJunkie.Users.User (User)
import qualified LearningJunkie.Users.User.Attributes as Attributes
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = Summary "Add a new user" :> ReqBody '[JSON] Attributes.New :> PostCreated '[JSON] User

handler :: Attributes.New -> AppM User
handler newUser = do
    addedUser <- insertUser newUser

    return $ toUserType addedUser
