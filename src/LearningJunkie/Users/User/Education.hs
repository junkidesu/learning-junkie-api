{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module LearningJunkie.Users.User.Education (Education (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import Database.Beam.Backend
import Database.Beam.Postgres (Postgres)
import GHC.Generics (Generic)

data Education = Bachelor | Master | PhD
        deriving (Show, Eq, Read, Generic)

instance FromJSON Education
instance ToJSON Education
instance ToSchema Education
instance FromBackendRow Postgres Education where
        fromBackendRow = read . T.unpack <$> fromBackendRow

instance (HasSqlValueSyntax be String) => HasSqlValueSyntax be Education where
        sqlValueSyntax = autoSqlValueSyntax
