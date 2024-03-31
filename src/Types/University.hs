{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Types.University (University (..)) where

import Control.Monad.Trans.Maybe
import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), RowParser, field)
import GHC.Generics (Generic)

data University = University
    { id :: !Int
    , name :: !Text
    , abbreviation :: !(Maybe Text)
    , year :: !Int
    , url :: !Text
    , logo :: !(Maybe Text)
    , joined :: !UTCTime
    }
    deriving (Eq, Show, Read, Generic)

instance FromJSON University
instance ToJSON University
instance FromRow University

instance FromRow (Maybe University) where
    fromRow :: RowParser (Maybe University)
    fromRow = do
        mbId <- field :: RowParser (Maybe Int)
        mbName <- field :: RowParser (Maybe Text)
        mbAbbr <- field :: RowParser (Maybe Text)
        mbYear <- field :: RowParser (Maybe Int)
        mbUrl <- field :: RowParser (Maybe Text)
        mbLogo <- field :: RowParser (Maybe Text)
        mbJoined <- field :: RowParser (Maybe UTCTime)

        runMaybeT $
            University
                <$> hoistMaybe mbId
                <*> hoistMaybe mbName
                <*> pure mbAbbr
                <*> hoistMaybe mbYear
                <*> hoistMaybe mbUrl
                <*> pure mbLogo
                <*> hoistMaybe mbJoined

instance ToRow University
instance ToSchema University
