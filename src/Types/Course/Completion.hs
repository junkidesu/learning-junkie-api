{-# LANGUAGE DeriveGeneric #-}

module Types.Course.Completion (Completion (..)) where

import Data.Aeson (ToJSON)
import Data.Swagger (ToSchema)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import GHC.Generics (Generic)
import Types.Course (Course)
import Types.User (User)

data Completion = Completion
    { course :: !Course
    , user :: !User
    , time :: !UTCTime
    }
    deriving (Show, Generic)

instance ToJSON Completion
instance ToSchema Completion
instance FromRow Completion where
    fromRow =
        Completion
            <$> fromRow
            <*> fromRow
            <*> field
