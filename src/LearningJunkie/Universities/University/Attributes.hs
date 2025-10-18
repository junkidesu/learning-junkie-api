{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module LearningJunkie.Universities.University.Attributes where

import Data.Aeson (FromJSON)
import Data.Functor.Identity (Identity)
import Data.Int (Int32)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import LearningJunkie.Attribute (Attribute)

data UniversityAttributes f = UniversityAttributes
    { name :: Attribute f Text
    , abbreviation :: Attribute f (Maybe Text)
    , year :: Attribute f Int32
    , url :: Attribute f Text
    , logo :: Attribute f (Maybe Text)
    }

type New = UniversityAttributes Identity
type Edit = UniversityAttributes Maybe

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
