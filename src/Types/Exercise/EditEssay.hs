{-# LANGUAGE DeriveGeneric #-}

module Types.Exercise.EditEssay (EditEssay (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)

data EditEssay = EditEssay
    { task :: !Text
    , model :: !Text
    }
    deriving (Show, Read, Generic)

instance FromJSON EditEssay
instance ToJSON EditEssay
instance ToSchema EditEssay
