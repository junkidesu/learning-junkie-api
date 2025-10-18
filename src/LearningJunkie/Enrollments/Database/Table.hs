{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module LearningJunkie.Enrollments.Database.Table where

import Data.Time (UTCTime)
import Database.Beam (Beamable, C, Generic, Identity, Table (PrimaryKey, primaryKey))
import LearningJunkie.Courses.Database.Table (CourseT)
import LearningJunkie.Users.Database.Table (UserT)

data EnrollmentT f = Enrollment
        { _enrollmentUser :: PrimaryKey UserT f
        , _enrollmentCourse :: PrimaryKey CourseT f
        , _enrollmentTime :: C f UTCTime
        }
        deriving (Generic, Beamable)

type Enrollment = EnrollmentT Identity
type EnrollmentId = PrimaryKey EnrollmentT Identity

deriving instance Show Enrollment
deriving instance Show EnrollmentId

instance Table EnrollmentT where
        data PrimaryKey EnrollmentT f
                = EnrollmentId
                        (PrimaryKey UserT f)
                        (PrimaryKey CourseT f)
                deriving (Generic, Beamable)
        primaryKey = EnrollmentId <$> _enrollmentUser <*> _enrollmentCourse
