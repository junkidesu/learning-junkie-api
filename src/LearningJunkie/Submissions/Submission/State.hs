{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module LearningJunkie.Submissions.Submission.State where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import Database.Beam (FromBackendRow (fromBackendRow))
import Database.Beam.Backend (HasSqlValueSyntax (sqlValueSyntax), autoSqlValueSyntax)
import Database.Beam.Postgres (Postgres)
import GHC.Generics

data SubmissionState = Submitted | Graded
    deriving (Generic, Show, Read, Eq, Ord)

instance FromJSON SubmissionState
instance ToJSON SubmissionState
instance ToSchema SubmissionState

instance FromBackendRow Postgres SubmissionState where
    fromBackendRow = read . T.unpack <$> fromBackendRow

instance (HasSqlValueSyntax be String) => HasSqlValueSyntax be SubmissionState where
    sqlValueSyntax = autoSqlValueSyntax
