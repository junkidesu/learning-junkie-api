{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Certificates.Web where

import qualified LearningJunkie.Certificates.Web.ById as ById
import qualified LearningJunkie.Certificates.Web.Verify as Verify
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
  "certificates"
    :> (ById.API :<|> Verify.API)

server :: ServerT API AppM
server =
  ById.handler
    :<|> Verify.handler
