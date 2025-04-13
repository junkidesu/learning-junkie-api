{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.All (API, handler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import LearningJunkie.Users.Database (selectAllUsers, toUserType)
import LearningJunkie.Users.User (User)
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = Summary "Get all users" :> Get '[JSON] [User]

handler :: AppM [User]
handler =
    map toUserType <$> selectAllUsers
