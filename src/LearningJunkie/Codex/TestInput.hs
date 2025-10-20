{-# LANGUAGE DeriveGeneric #-}

module LearningJunkie.Codex.TestInput where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import LearningJunkie.Codex.Environment (Environment)

data TestInput = TestInput
        { program :: Text
        , environment :: Environment
        }
        deriving (Generic)

instance FromJSON TestInput
instance ToJSON TestInput
instance ToSchema TestInput
