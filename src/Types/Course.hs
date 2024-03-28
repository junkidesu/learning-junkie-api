{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Types.Course (Course (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromRow
import GHC.Generics (Generic)
import Types.Course.Difficulty (Difficulty)
import Types.University (University)
import Types.User (User)

data Course = Course
    { id :: !Int
    , title :: !Text
    , description :: !Text
    , difficulty :: !Difficulty
    , university :: !University
    , instructor :: !User
    , totalPoints :: !Int
    , enrollmentsCount :: !Int
    }
    deriving (Show, Eq, Read, Generic)

instance FromJSON Course
instance ToJSON Course
instance FromRow Course where
    fromRow :: RowParser Course
    fromRow =
        Course
            <$> field -- id
            <*> field -- title
            <*> field -- description
            <*> field -- difficulty
            <*> fromRow -- university
            <*> fromRow -- instructor
            <*> field -- totalPoints
            <*> field -- enrollmentsCount

instance ToSchema Course
