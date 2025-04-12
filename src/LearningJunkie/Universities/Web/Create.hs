{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Universities.Web.Create (API, handler) where

import LearningJunkie.Universities.Database (insertUniversity, toUniversityType)
import LearningJunkie.Universities.University (University)
import qualified LearningJunkie.Universities.University.Attributes as Attributes
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    Summary "Add a university"
        :> ReqBody' '[Required] '[JSON] Attributes.New
        :> PostCreated '[JSON] University

handler :: Attributes.New -> AppM University
handler newUniversity =
    toUniversityType <$> insertUniversity newUniversity
