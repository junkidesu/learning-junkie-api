{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module LearningJunkie.Courses.Course.Difficulty where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import Database.Beam (FromBackendRow (fromBackendRow))
import Database.Beam.Backend (HasSqlValueSyntax (sqlValueSyntax), autoSqlValueSyntax)
import Database.Beam.Postgres (Postgres)
import GHC.Generics (Generic)

data Difficulty = Beginner | Intermediate | Advanced
        deriving (Show, Eq, Read, Generic)

instance FromJSON Difficulty
instance ToJSON Difficulty
instance ToSchema Difficulty
instance FromBackendRow Postgres Difficulty where
        fromBackendRow = read . T.unpack <$> fromBackendRow

instance (HasSqlValueSyntax be String) => HasSqlValueSyntax be Difficulty where
        sqlValueSyntax = autoSqlValueSyntax
