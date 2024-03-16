{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.User.Education (Education (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 (unpack)
import Data.Swagger (ToSchema)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField (toField))
import Database.PostgreSQL.Simple.TypeInfo.Static (text)
import GHC.Generics (Generic)

data Education = Bachelor | Master | PhD
    deriving (Show, Eq, Read, Generic)

instance FromJSON Education
instance ToJSON Education
instance ToSchema Education

instance ToField Education where
    toField :: Education -> Action
    toField Bachelor = Escape "bachelor"
    toField Master = Escape "master"
    toField PhD = Escape "phd"

instance FromField Education where
    fromField :: FieldParser Education
    fromField f mdata =
        if typeOid f /= typoid text
            then returnError Incompatible f ""
            else case unpack <$> mdata of
                Nothing -> returnError UnexpectedNull f ""
                Just dat -> case dat of
                    "bachelor" -> pure Bachelor
                    "master" -> pure Master
                    "phd" -> pure PhD
                    _ -> returnError ConversionFailed f ""
