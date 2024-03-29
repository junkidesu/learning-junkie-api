{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Exercise.Quiz (Quiz (..)) where

import Control.Lens hiding (Choice, (.=))
import Data.Aeson
import Data.Data (Proxy (Proxy))
import Data.Swagger hiding (options, title)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromRow
import GHC.Generics (Generic)
import Types.Course (Course)
import Types.Exercise.Choice
import Prelude hiding (id)

data Quiz = Quiz
    { id :: !Int
    , title :: !(Maybe Text)
    , grade :: !Int
    , question :: !Text
    , options :: Choice -> Text
    , correct :: Choice
    , course :: !Course
    }
    deriving (Generic)

instance FromJSON Quiz

instance ToJSON Quiz where
    toJSON :: Quiz -> Value
    toJSON q =
        object
            [ "id" .= id q
            , "title" .= title q
            , "grade" .= grade q
            , "question" .= question q
            , "options" .= options q
            , "course" .= course q
            ]

instance ToSchema Quiz where
    declareNamedSchema _ = do
        intSchema <- declareSchemaRef (Proxy :: Proxy Int)
        textSchema <- declareSchemaRef (Proxy :: Proxy Text)
        optionsSchema <- declareSchemaRef (Proxy :: Proxy (Choice -> Text))
        courseSchema <- declareSchemaRef (Proxy :: Proxy Course)

        return $
            NamedSchema (Just "Quiz") $
                mempty
                    & properties
                        .~ [ ("id", intSchema)
                           , ("title", textSchema)
                           , ("grade", intSchema)
                           , ("question", textSchema)
                           , ("options", optionsSchema)
                           , ("course", courseSchema)
                           ]
                    & required .~ ["id", "grade", "question", "options", "course"]

instance FromRow Quiz where
    fromRow :: RowParser Quiz
    fromRow =
        Quiz
            <$> field -- id
            <*> field -- title
            <*> field -- grade
            <*> field -- question
            <*> fromRow -- options
            <*> field -- correct
            <*> fromRow -- course
