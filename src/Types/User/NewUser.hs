{-# LANGUAGE DeriveGeneric #-}

module Types.User.NewUser (NewUser (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import Types.User.Education (Education)
import Types.User.Role (Role)

data NewUser = NewUser
    { name :: !(Maybe Text)
    , birthday :: !(Maybe Day)
    , education :: !(Maybe Education)
    , role :: !Role
    , email :: !Text
    , password :: !Text
    }
    deriving (Eq, Show, Generic)

instance FromJSON NewUser
instance ToJSON NewUser
instance ToSchema NewUser
