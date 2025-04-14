{-# LANGUAGE DeriveGeneric #-}

module LearningJunkie.Users.User.Login.AuthUser (AuthUser (AuthUser)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import Data.Text (Text)
import GHC.Generics (Generic)
import LearningJunkie.Users.User.Role (Role)
import Servant.Auth.JWT (FromJWT, ToJWT)

data AuthUser = AuthUser
    { id :: !Int32
    , email :: !Text
    , role :: !Role
    }
    deriving (Show, Read, Generic)

instance ToJSON AuthUser
instance FromJSON AuthUser
instance ToJWT AuthUser
instance FromJWT AuthUser
