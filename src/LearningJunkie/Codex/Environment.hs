{-# LANGUAGE DeriveGeneric #-}

module LearningJunkie.Codex.Environment (Environment (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)

data Environment = Haskell | Node | Python
        deriving (Show, Read, Eq, Generic)

instance FromJSON Environment
instance ToJSON Environment
instance ToSchema Environment
