{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module LearningJunkie.Exercises.Exercise.Content where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text
import GHC.Generics (Generic)
import LearningJunkie.Exercises.Exercise.Quiz.Option (Option)

data Content
    = TypeAnswer
        { question :: Text
        , answer :: Text
        }
    | TrueFalse
        { question :: Text
        , correctBool :: Bool
        }
    | Essay
        { task :: Text
        , model :: Text
        }
    | Quiz
        { question :: Text
        , options :: Option -> Text
        , correctOption :: Option
        }
    deriving (Show, Generic)

instance ToJSON Content
instance FromJSON Content
instance ToSchema Content
