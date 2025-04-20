{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.Register where

import LearningJunkie.Users.Database (insertUser, toUserType)
import qualified LearningJunkie.Users.Database.Role as Role
import LearningJunkie.Users.User (User)
import qualified LearningJunkie.Users.User.Attributes as Attributes
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    "register"
        :> Summary "Register as a student to the application"
        :> ReqBody '[JSON] Attributes.New
        :> PostCreated '[JSON] User

handler :: Attributes.New -> AppM User
handler newUser =
    toUserType <$> insertUser newUser Role.Student Nothing
