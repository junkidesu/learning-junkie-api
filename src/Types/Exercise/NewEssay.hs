{-# LANGUAGE DeriveGeneric #-}

module Types.Exercise.NewEssay (NewEssay (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)

data NewEssay = NewEssay
    { grade :: !Int
    , task :: !Text
    , model :: !Text
    }
    deriving (Show, Read, Generic)

instance FromJSON NewEssay
instance ToJSON NewEssay
instance ToSchema NewEssay
