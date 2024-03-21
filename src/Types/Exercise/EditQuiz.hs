{-# LANGUAGE DeriveGeneric #-}

module Types.Exercise.EditQuiz (EditQuiz (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text
import GHC.Generics
import Types.Exercise.Choice

data EditQuiz = EditQuiz
    { question :: !Text
    , options :: Choice -> Text
    , correct :: Choice
    }
    deriving (Generic)

instance FromJSON EditQuiz
instance ToJSON EditQuiz
instance ToSchema EditQuiz
