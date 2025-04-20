{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Universities.Web.Instructors.All where

import Data.Int (Int32)
import LearningJunkie.Users.Database (selectUniversityInstructors, toUserType)
import LearningJunkie.Users.User (User)
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = Summary "Get all university instructors" :> Get '[JSON] [User]

handler :: Int32 -> AppM [User]
handler universityId =
    map toUserType
        <$> selectUniversityInstructors
            universityId
