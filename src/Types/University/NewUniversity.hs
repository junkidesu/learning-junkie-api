{-# LANGUAGE DeriveGeneric #-}

module Types.University.NewUniversity where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Database.PostgreSQL.Simple (ToRow)
import GHC.Generics (Generic)

data NewUniversity = NewUniversity
    { name :: !Text
    , abbreviation :: !(Maybe Text)
    , year :: !Int
    }
    deriving (Show, Eq, Read, Generic)

instance FromJSON NewUniversity
instance ToJSON NewUniversity
instance ToRow NewUniversity
instance ToSchema NewUniversity
