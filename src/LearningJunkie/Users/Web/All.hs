{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.All (API, handler) where

import LearningJunkie.Users.Database (selectAllUsers, toUserType)
import LearningJunkie.Users.User (User)
import LearningJunkie.Web.AppM (AppM)
import Servant 
import Control.Monad.IO.Class (MonadIO(liftIO))

type API = Summary "Get all users" :> Get '[JSON] [User]

handler :: AppM [User]
handler = do 
    users <- map toUserType <$> selectAllUsers

    liftIO $ print users 

    return users
