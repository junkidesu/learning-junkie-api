{-# LANGUAGE DeriveGeneric #-}

module LearningJunkie.Web.Auth.User (AuthUser (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import Data.Text (Text)
import GHC.Generics (Generic)
import LearningJunkie.Users.Database.Role (Role)
import Servant.Auth.JWT (FromJWT, ToJWT)

data AuthUser = AuthUser
    { id :: !Int32
    , email :: !Text
    , role :: !Role
    , university :: !(Maybe Int32)
    }
    deriving (Show, Read, Generic)

instance ToJSON AuthUser
instance FromJSON AuthUser
instance ToJWT AuthUser
instance FromJWT AuthUser
