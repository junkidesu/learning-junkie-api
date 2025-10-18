{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module LearningJunkie.Lessons.Lesson.Attributes where

import Data.Aeson (FromJSON)
import Data.Functor.Identity
import Data.Int (Int32)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import LearningJunkie.Attribute (Attribute)
import LearningJunkie.Lessons.Lesson.Component (Component)

data LessonAttributes f = LessonAttributes
    { number :: Attribute f Int32
    , title :: Attribute f Text
    , description :: Attribute f Text
    , components :: Attribute f [Component]
    }

type New = LessonAttributes Identity
type Edit = LessonAttributes Maybe

deriving instance Show New
deriving instance Generic New
deriving instance FromJSON New
deriving instance ToSchema New

deriving instance Show Edit
deriving instance Generic Edit
deriving instance FromJSON Edit
deriving instance ToSchema Edit
