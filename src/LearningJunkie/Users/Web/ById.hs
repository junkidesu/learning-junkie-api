{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.ById where

import Data.Int (Int32)
import LearningJunkie.Users.Database (selectUserById, toUserType, userByIdQuery)
import LearningJunkie.Users.User (User)
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = Summary "Get user by ID" :> Capture' '[Required, Description ""] "id" Int32 :> Get '[JSON] User

handler :: Int32 -> AppM User
handler userId = do
    mbUser <- selectUserById userId

    case mbUser of
        Nothing -> throwError err404{errBody = "No user with such ID"}
        Just user -> return $ toUserType user
