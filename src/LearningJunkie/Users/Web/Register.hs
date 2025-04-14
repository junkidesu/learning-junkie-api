{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.Register where

import LearningJunkie.Users.Database (insertUser, toUserType)
import LearningJunkie.Users.User (User)
import qualified LearningJunkie.Users.User.Attributes as Attributes
import LearningJunkie.Users.User.Role (Role (Student))
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    "register"
        :> Summary "Register as a student to the application"
        :> ReqBody '[JSON] Attributes.New
        :> PostCreated '[JSON] User

handler :: Attributes.New -> AppM User
handler newUser = do
    case Attributes.role newUser of
        Student -> do
            addedUser <- insertUser newUser

            return $ toUserType addedUser
        _ -> throwError err401{errBody = "Only students can register; contact admin to register as an instructor"}
