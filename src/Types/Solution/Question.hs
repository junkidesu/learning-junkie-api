{-# LANGUAGE DeriveGeneric #-}

module Types.Solution.Question (Solution (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Solution = Solution
    { answer :: Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON Solution
instance ToJSON Solution
instance ToSchema Solution
