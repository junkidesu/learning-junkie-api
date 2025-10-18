{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Universities.Web.ById (API, handler) where

import Data.Int (Int32)
import LearningJunkie.Universities.Database (selectUniversityById, toUniversityType)
import LearningJunkie.Universities.University (University)
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    Summary "Get university by ID"
        :> Capture' '[Required, Description "ID of the university"] "id" Int32
        :> Get '[JSON] University

handler :: Int32 -> AppM University
handler universityId = do
    mbUniversity <- selectUniversityById universityId

    case mbUniversity of
        Nothing -> throwError err404
        Just university ->
            return $ toUniversityType university
