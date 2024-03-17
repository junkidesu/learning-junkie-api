{-# LANGUAGE DeriveGeneric #-}

module Types.Instructor (Instructor (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text
import Data.Time
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import GHC.Generics
import Types.University (University)
import Types.User.Education
import Types.User.Role (Role)

data Instructor = Instructor
    { id :: !Int
    , joined :: !UTCTime
    , name :: !(Maybe Text)
    , birthday :: !(Maybe Day)
    , education :: !(Maybe Education)
    , role :: Role
    , email :: !Text
    , university :: !University
    }
    deriving (Eq, Read, Show, Generic)

instance FromJSON Instructor
instance ToJSON Instructor
instance ToSchema Instructor
instance FromRow Instructor where
    fromRow =
        Instructor
            <$> field -- id
            <*> field -- joined
            <*> field -- name
            <*> field -- birthday
            <*> field -- education
            <*> field -- role
            <*> field -- email
            <*> fromRow -- university
