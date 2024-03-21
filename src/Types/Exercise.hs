{-# LANGUAGE DeriveGeneric #-}

module Types.Exercise (Exercise (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import GHC.Generics (Generic)
import Types.Course (Course)

data Exercise = Exercise
    { id :: !Int
    , title :: !(Maybe Text)
    , grade :: !Int
    , course :: !Course
    }
    deriving (Generic)

instance FromRow Exercise where
    fromRow =
        Exercise
            <$> field
            <*> field
            <*> field
            <*> fromRow

instance FromJSON Exercise
instance ToJSON Exercise
instance ToSchema Exercise
