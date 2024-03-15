{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Course.Difficulty (Difficulty (..)) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Char8 as BS
import Data.Swagger (ToSchema)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField (toField))
import Database.PostgreSQL.Simple.TypeInfo.Static (text)
import GHC.Generics (Generic)

data Difficulty = Beginner | Intermediate | Advanced
    deriving (Show, Eq, Read, Generic)

instance FromJSON Difficulty
instance ToJSON Difficulty
instance ToSchema Difficulty

instance ToField Difficulty where
    toField :: Difficulty -> Action
    toField Beginner = Escape "beginner"
    toField Intermediate = Escape "intermediate"
    toField Advanced = Escape "advanced"

instance FromField Difficulty where
    fromField :: FieldParser Difficulty
    fromField f mdata =
        if typeOid f /= typoid text
            then returnError Incompatible f ""
            else case BS.unpack <$> mdata of
                Nothing -> returnError UnexpectedNull f ""
                Just dat -> case dat of
                    "beginner" -> pure Beginner
                    "intermediate" -> pure Intermediate
                    "advanced" -> pure Advanced
                    _ -> returnError ConversionFailed f ""
