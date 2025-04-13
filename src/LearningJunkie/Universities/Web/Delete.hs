{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Universities.Web.Delete (API, handler) where

import Data.Int (Int32)
import LearningJunkie.Universities.Database (deleteUniversity)
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    Summary "Delete university by ID"
        -- :> JWTAuth
        :> Capture' '[Required, Description "ID of the unviersity"] "id" Int32
        :> Verb 'DELETE 204 '[JSON] NoContent

handler :: Int32 -> AppM NoContent
handler universityId = do
    deleteUniversity universityId
    return NoContent
