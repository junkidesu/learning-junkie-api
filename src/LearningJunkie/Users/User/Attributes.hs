{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module LearningJunkie.Users.User.Attributes where

import Data.Aeson (FromJSON)
import Data.Functor.Identity (Identity)
import Data.Int (Int32)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import LearningJunkie.Attribute (Attribute)
import LearningJunkie.Users.User.Education (Education)
import LearningJunkie.Users.User.Role (Role)

data UserAttributes f = UserAttributes
        { name :: Attribute f Text
        , birthday :: Attribute f (Maybe Day)
        , education :: Attribute f (Maybe Education)
        , role :: Attribute f Role
        , email :: Attribute f Text
        , avatar :: Attribute f (Maybe Text)
        , password :: Attribute f Text
        , university :: Attribute f (Maybe Int32)
        }

type New = UserAttributes Identity
type Edit = UserAttributes Maybe

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
