{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.Self where

import qualified LearningJunkie.Users.Web.Self.Avatar as Avatar
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = "self" :> (Avatar.API)

server :: ServerT API AppM
server = Avatar.server
