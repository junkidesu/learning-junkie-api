{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module LearningJunkie.Users.Database.Table where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Database.Beam
import LearningJunkie.Universities.Database.Table (UniversityT)
import LearningJunkie.Users.Database.Education (Education)
import LearningJunkie.Users.Database.Role (Role)

data UserT f = User
        { _userId :: C f Int32
        , _userJoined :: C f UTCTime
        , _userName :: C f Text
        , _userBirthday :: C f (Maybe Day)
        , _userEducation :: C f (Maybe Education)
        , _userRole :: C f Role
        , _userEmail :: C f Text
        , _userAvatar :: C f (Maybe Text)
        , _userPasswordHash :: C f Text
        , _userUniversity :: PrimaryKey UniversityT (Nullable f)
        }
        deriving (Generic, Beamable)

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Show UserId

instance Table UserT where
        data PrimaryKey UserT f = UserId (C f Int32) deriving (Generic, Beamable)
        primaryKey = UserId . _userId
