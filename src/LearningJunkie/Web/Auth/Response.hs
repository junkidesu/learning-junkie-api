{-# LANGUAGE DeriveGeneric #-}

module LearningJunkie.Web.Auth.Response (AuthResponse (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)

data AuthResponse = AuthResponse
        { id :: !Int32
        , token :: !Text
        }
        deriving (Show, Eq, Generic)

instance ToJSON AuthResponse
instance FromJSON AuthResponse
instance ToSchema AuthResponse
