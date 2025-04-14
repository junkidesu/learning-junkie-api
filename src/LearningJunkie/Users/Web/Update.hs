{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.Update where

import Data.Int (Int32)
import LearningJunkie.Users.Database (toUserType, updateUser)
import LearningJunkie.Users.User
import qualified LearningJunkie.Users.User.Attributes as Attributes
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    Summary "Update a user by ID"
        :> Capture' '[Required, Description "ID of the user"] "id" Int32
        :> ReqBody '[JSON] Attributes.Edit
        :> Put '[JSON] User

handler :: Int32 -> Attributes.Edit -> AppM User
handler userId editUser = toUserType <$> updateUser userId editUser
