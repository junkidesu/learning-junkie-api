{-# LANGUAGE DeriveGeneric #-}

module LearningJunkie.Codex.ExecutionResult (ExecutionResult (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)

data ExecutionResult
        = Success
                { executionOutput :: Text
                }
        | Failure
                { executionError :: Text
                }
        deriving (Show, Read, Eq, Generic)

instance FromJSON ExecutionResult
instance ToJSON ExecutionResult
instance ToSchema ExecutionResult
