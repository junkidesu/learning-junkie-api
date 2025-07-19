{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module LearningJunkie.Exercises.Exercise.Attributes where

import Data.Aeson
import Data.Functor.Identity (Identity)
import Data.Int (Int32)
import Data.OpenApi
import Data.Text
import GHC.Generics (Generic)
import LearningJunkie.Attribute (Attribute)
import LearningJunkie.Exercises.Exercise.Content (Content)

data ExerciseAttributes f
    = ExerciseAttributes
    { title :: Attribute f Text
    , description :: Attribute f Text
    , maxGrade :: Attribute f Int32
    , content :: Attribute f Content
    }

type New = ExerciseAttributes Identity
type Edit = ExerciseAttributes Maybe

deriving instance Show New
deriving instance Generic New
deriving instance FromJSON New
deriving instance ToJSON New
deriving instance ToSchema New

deriving instance Show Edit
deriving instance Generic Edit
deriving instance FromJSON Edit
deriving instance ToJSON Edit
deriving instance ToSchema Edit
