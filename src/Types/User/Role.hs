{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.User.Role (Role (..)) where

import Data.Aeson
import Data.ByteString.Char8 (unpack)
import Data.Swagger (ToSchema)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.TypeInfo.Static (text)
import GHC.Generics (Generic)

data Role = Admin | Student | Instructor
    deriving (Read, Show, Eq, Generic)

instance FromJSON Role
instance ToJSON Role

instance FromField Role where
    fromField :: FieldParser Role
    fromField field mdata =
        if typeOid field /= typoid text
            then returnError Incompatible field ""
            else case unpack <$> mdata of
                Nothing -> returnError UnexpectedNull field ""
                Just dat -> case dat of
                    "admin" -> pure Admin
                    "student" -> pure Student
                    "instructor" -> pure Instructor
                    _ -> returnError ConversionFailed field dat

instance ToField Role where
    toField :: Role -> Action
    toField Admin = Escape "admin"
    toField Student = Escape "student"
    toField Instructor = Escape "instructor"

instance ToSchema Role
