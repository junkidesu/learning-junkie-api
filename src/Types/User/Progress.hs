{-# LANGUAGE DeriveGeneric #-}

module Types.User.Progress (Progress (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import GHC.Generics (Generic)
import Types.Course (Course)

data Progress = Progress
    { course :: !Course
    , solvedExercises :: !Int
    }
    deriving (Show, Generic)

instance ToJSON Progress
instance FromJSON Progress
instance ToSchema Progress

instance FromRow Progress where
    fromRow =
        Progress
            <$> fromRow
            <*> field
