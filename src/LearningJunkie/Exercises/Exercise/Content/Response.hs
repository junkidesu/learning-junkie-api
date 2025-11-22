{-# LANGUAGE DeriveGeneric #-}

module LearningJunkie.Exercises.Exercise.Content.Response where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import LearningJunkie.Codex.Environment (Environment)
import qualified LearningJunkie.Exercises.Exercise.Content as Content
import LearningJunkie.Exercises.Exercise.Quiz.Option (Option)

data ContentResponse
    = TypeAnswer
        { question :: Text
        }
    | TrueFalse
        { question :: Text
        }
    | Essay
        { task :: Text
        }
    | Quiz
        { question :: Text
        , options :: Option -> Text
        }
    | Project
        { requirements :: Text
        }
    | Coding
        { requirements :: Text
        , environment :: Environment
        }
    deriving (Show, Generic)

toContentResponse :: Content.Content -> ContentResponse
toContentResponse (Content.TypeAnswer q _) = TypeAnswer q
toContentResponse (Content.TrueFalse q _) = TrueFalse q
toContentResponse (Content.Essay t _) = Essay t
toContentResponse (Content.Quiz q o _) = Quiz q o
toContentResponse (Content.Project r) = Project r
toContentResponse (Content.Coding r e _ _) = Coding r e

instance ToJSON ContentResponse
instance FromJSON ContentResponse
instance ToSchema ContentResponse
