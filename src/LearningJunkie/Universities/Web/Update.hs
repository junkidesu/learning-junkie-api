{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Universities.Web.Update (API, handler) where

import Data.Int (Int32)
import LearningJunkie.Universities.Database (toUniversityType, updateUniversity)
import LearningJunkie.Universities.University (University)
import qualified LearningJunkie.Universities.University.Attributes as Attributes
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    Summary "Update university by ID"
        :> Capture' '[Required, Description "ID of the university"] "id" Int32
        :> ReqBody '[JSON] Attributes.Edit
        :> Put '[JSON] University

handler :: Int32 -> Attributes.Edit -> AppM University
handler universityId editUniversity = toUniversityType <$> updateUniversity universityId editUniversity
