{-# LANGUAGE DeriveGeneric #-}

module Types.Exercise.NewQuiz (NewQuiz (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text
import GHC.Generics
import Types.Exercise.Choice

data NewQuiz = NewQuiz
    { grade :: !Int
    , question :: !Text
    , options :: Choice -> Text
    , correct :: Choice
    }
    deriving (Generic)

instance FromJSON NewQuiz
instance ToJSON NewQuiz
instance ToSchema NewQuiz
