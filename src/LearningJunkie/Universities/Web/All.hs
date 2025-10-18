{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Universities.Web.All (API, handler) where

import LearningJunkie.Universities.Database (selectAllUniversities, toUniversityType)
import LearningJunkie.Universities.University (University)
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = Summary "Get all universities" :> Get '[JSON] [University]

handler :: AppM [University]
handler =
    map toUniversityType
        <$> selectAllUniversities
