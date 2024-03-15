{-# LANGUAGE DeriveGeneric #-}

module Types.Auth.Response (AuthResponse (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)

data AuthResponse = AuthResponse
    { id :: !Int
    , token :: !Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON AuthResponse
instance FromJSON AuthResponse
instance ToSchema AuthResponse
