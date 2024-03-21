{-# LANGUAGE DeriveGeneric #-}

module Types.Exercise.Question (Question (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow)
import GHC.Generics (Generic)

data Question = Question
    { id :: !Int
    , title :: !(Maybe Text)
    , grade :: !Int
    , question :: !Text
    , answer :: !Text
    }
    deriving (Show, Read, Generic)

instance FromJSON Question
instance ToJSON Question
instance FromRow Question
instance ToSchema Question
