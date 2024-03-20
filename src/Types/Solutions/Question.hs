{-# LANGUAGE DeriveGeneric #-}

module Types.Solutions.Question where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)

data QuestionSolution = QuestionSolution
    { answer :: !Text
    }
    deriving (Show, Eq, Read, Generic)

instance FromJSON QuestionSolution
instance ToJSON QuestionSolution
instance ToSchema QuestionSolution
