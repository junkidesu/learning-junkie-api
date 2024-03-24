{-# LANGUAGE DeriveGeneric #-}

module Types.User.Progress (Progress (..), courseFinished) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import GHC.Generics (Generic)
import Types.Course (Course)
import qualified Types.Course as C

data Progress = Progress
    { course :: !Course
    , obtainedPoints :: !Int
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

courseFinished :: Progress -> Bool
courseFinished progress = obtainedPoints progress == (C.totalPoints . course $ progress)
