{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module LearningJunkie.Universities.University.Attributes (
    Attributes (..),
    New,
    Edit,
) where

import Data.Aeson (FromJSON)
import Data.Int (Int32)
import Data.Swagger (ToSchema)
import Data.Text
import Database.Beam (Identity)
import GHC.Generics (Generic)
import LearningJunkie.Attribute (Attribute)

data Attributes f = Attributes
    { name :: !(Attribute f Text)
    , abbreviation :: !(Attribute f (Maybe Text))
    , year :: !(Attribute f Int32)
    , url :: !(Attribute f Text)
    , logo :: !(Attribute f (Maybe Text))
    }

type New = Attributes Identity
type Edit = Attributes Maybe

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
