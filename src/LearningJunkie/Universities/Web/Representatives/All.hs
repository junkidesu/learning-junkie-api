{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Universities.Web.Representatives.All where

import Data.Int (Int32)
import LearningJunkie.Users.Database (selectUniversityRepresentatives, toUserType)
import LearningJunkie.Users.User (User)
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = Summary "Get all university representatives" :> Get '[JSON] [User]

handler :: Int32 -> AppM [User]
handler universityId =
    map toUserType
        <$> selectUniversityRepresentatives
            universityId
