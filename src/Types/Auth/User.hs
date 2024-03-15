{-# LANGUAGE DeriveGeneric #-}

module Types.Auth.User (AuthUser (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.Auth.JWT (FromJWT, ToJWT)
import Types.User.Role (Role)

data AuthUser = AuthUser
    { id :: !Int
    , email :: !Text
    , role :: !Role
    }
    deriving (Show, Read, Generic)

instance ToJSON AuthUser
instance FromJSON AuthUser
instance ToJWT AuthUser
instance FromJWT AuthUser
