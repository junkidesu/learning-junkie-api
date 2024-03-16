{-# LANGUAGE DeriveGeneric #-}

module Types.Course (Course (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Database.PostgreSQL.Simple.ToRow (ToRow (toRow))
import GHC.Generics (Generic)
import Types.Course.Difficulty (Difficulty)
import Types.University (University (University))

data Course = Course
    { id :: !Int
    , title :: !Text
    , description :: !Text
    , difficulty :: !Difficulty
    , university :: !University
    }
    deriving (Show, Eq, Read, Generic)

instance FromJSON Course
instance ToJSON Course
instance FromRow Course where
    fromRow =
        Course
            <$> field
            <*> field
            <*> field
            <*> field
            <*> ( University
                    <$> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field
                )

instance ToRow Course where
    toRow = undefined

instance ToSchema Course
