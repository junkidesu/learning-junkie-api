{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Exercise.Question (Question (..)) where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Data (Proxy (Proxy))
import Data.Swagger (HasProperties (properties), HasRequired (required), NamedSchema (NamedSchema), ToSchema (declareNamedSchema), declareSchemaRef)
import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow)
import GHC.Generics (Generic)
import Prelude hiding (id)

data Question = Question
    { id :: !Int
    , title :: !(Maybe Text)
    , grade :: !Int
    , question :: !Text
    , answer :: !Text
    }
    deriving (Show, Read, Generic)

instance FromJSON Question

instance ToJSON Question where
    toJSON :: Question -> Value
    toJSON q =
        object
            [ "id" .= id q
            , "title" .= title q
            , "grade" .= grade q
            , "question" .= question q
            ]

instance FromRow Question

instance ToSchema Question where
    declareNamedSchema _ = do
        intSchema <- declareSchemaRef (Proxy :: Proxy Int)
        textSchema <- declareSchemaRef (Proxy :: Proxy Text)

        return $
            NamedSchema (Just "Question") $
                mempty
                    & properties
                        .~ [ ("id", intSchema)
                           , ("title", textSchema)
                           , ("grade", intSchema)
                           , ("question", textSchema)
                           ]
                    & required .~ ["id", "grade", "question"]
