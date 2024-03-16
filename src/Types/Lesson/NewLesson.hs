{-# LANGUAGE DeriveGeneric #-}

module Types.Lesson.NewLesson (NewLesson (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Database.PostgreSQL.Simple (ToRow)
import GHC.Generics (Generic)

data NewLesson = NewLesson
    { number :: !Int
    , title :: !Text
    , description :: !Text
    , content :: !Text
    , courseId :: !Int
    }
    deriving (Show, Read, Generic)

instance FromJSON NewLesson
instance ToJSON NewLesson
instance ToRow NewLesson
instance ToSchema NewLesson
