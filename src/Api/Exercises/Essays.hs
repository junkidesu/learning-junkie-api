{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Exercises.Essays (EssaysAPI, essaysServer) where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Types.Auth.JWTAuth (JWTAuth)
import Types.Exercise.Essay (Essay)
import qualified Types.Exercise.NewEssay as NE

type GetEssayById =
  Summary "Get essay by ID"
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> Get '[JSON] Essay

type EditEssay =
  Summary "Edit essay with given ID"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the exercise"] "id" Int
    :> ReqBody '[JSON] NE.NewEssay
    :> Put '[JSON] Essay

type EssaysAPI =
  "essays"
    :> (GetEssayById :<|> EditEssay)

essaysServer :: Pool Connection -> Server EssaysAPI
essaysServer conns = undefined
