{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module LearningJunkie.Courses.Course.Attributes where

import Data.Aeson (FromJSON)
import Data.Functor.Identity (Identity)
import Data.Int (Int32)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import LearningJunkie.Attribute (Attribute)
import LearningJunkie.Courses.Course.CompletionRequirements (CompletionRequirements)
import LearningJunkie.Courses.Course.Difficulty (Difficulty)

data CourseAttributes f = CourseAttributes
        { title :: Attribute f Text
        , description :: Attribute f Text
        , difficulty :: Attribute f Difficulty
        , banner :: Attribute f (Maybe Text)
        , instructor :: Attribute f Int32
        , completionRequirements :: Attribute f CompletionRequirements
        }

type New = CourseAttributes Identity
type Edit = CourseAttributes Maybe

deriving instance Show New
deriving instance Eq New
deriving instance Generic New
deriving instance FromJSON New
deriving instance ToSchema New

deriving instance Show Edit
deriving instance Eq Edit
deriving instance Generic Edit
deriving instance FromJSON Edit
deriving instance ToSchema Edit
