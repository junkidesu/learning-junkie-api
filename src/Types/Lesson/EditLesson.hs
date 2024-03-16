{-# LANGUAGE DeriveGeneric #-}

module Types.Lesson.EditLesson (EditLesson (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)

data EditLesson = EditLesson
    { description :: !Text
    , content :: !Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON EditLesson
instance ToJSON EditLesson
instance ToSchema EditLesson
