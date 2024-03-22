{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Types.Exercise.Quiz (Quiz (..)) where

import Data.Aeson
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromRow
import GHC.Generics (Generic)
import Types.Exercise.Choice

data Quiz = Quiz
    { id :: !Int
    , title :: !(Maybe Text)
    , grade :: !Int
    , question :: !Text
    , options :: Choice -> Text
    , correct :: Choice
    }
    deriving (Generic)

instance FromJSON Quiz
instance ToJSON Quiz
instance ToSchema Quiz
instance FromRow Quiz where
    fromRow :: RowParser Quiz
    fromRow =
        Quiz
            <$> field -- id
            <*> field -- title
            <*> field -- grade
            <*> field -- question
            <*> fromRow -- options
            <*> field -- correct
