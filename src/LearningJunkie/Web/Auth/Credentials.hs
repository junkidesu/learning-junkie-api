{-# LANGUAGE DeriveGeneric #-}

module LearningJunkie.Web.Auth.Credentials (Credentials (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)

data Credentials = Credentials
        { email :: !Text
        , password :: !Text
        }
        deriving (Show, Read, Generic)

instance FromJSON Credentials
instance ToJSON Credentials
instance ToSchema Credentials
