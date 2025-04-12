{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module LearningJunkie.Universities.Database.Table where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Beam

data UniversityT f = University
        { _universityId :: C f Int32
        , _universityName :: C f Text
        , _universityAbbreviation :: C f (Maybe Text)
        , _universityYear :: C f Int32
        , _universityUrl :: C f Text
        , _universityLogo :: C f (Maybe Text)
        , _universityJoined :: C f UTCTime
        }
        deriving (Generic, Beamable)

type University = UniversityT Identity
type UniversityId = PrimaryKey UniversityT Identity

deriving instance Show University
deriving instance Show UniversityId
deriving instance Show (PrimaryKey UniversityT (Nullable Identity))

instance Table UniversityT where
        data PrimaryKey UniversityT f = UniversityId (C f Int32) deriving (Generic, Beamable)
        primaryKey = UniversityId . _universityId
