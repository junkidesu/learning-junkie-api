{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module LearningJunkie.Users.Database.Role (Role (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import Database.Beam (HasSqlEqualityCheck)
import Database.Beam.Backend
import Database.Beam.Postgres (Postgres)
import GHC.Generics (Generic)

data Role = Student | Instructor | UniversityRep | Admin
        deriving (Read, Show, Eq, Generic)

instance FromJSON Role
instance ToJSON Role
instance ToSchema Role

instance FromBackendRow Postgres Role where
        fromBackendRow = read . T.unpack <$> fromBackendRow

instance (HasSqlValueSyntax be String) => HasSqlValueSyntax be Role where
        sqlValueSyntax = autoSqlValueSyntax

instance (BeamSqlBackend be, HasSqlEqualityCheck be String) => HasSqlEqualityCheck be Role
