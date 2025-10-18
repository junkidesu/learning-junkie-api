{-# LANGUAGE DeriveGeneric #-}

module LearningJunkie.Lessons.Lesson.Component where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)

data Component
    = Markdown {content :: Text}
    | Video
        { source :: Text
        , title :: Text
        , description :: Text
        }
    deriving (Show, Generic)

instance FromJSON Component
instance ToJSON Component
instance ToSchema Component
