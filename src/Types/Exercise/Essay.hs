{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Exercise.Essay (Essay (..)) where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Data (Proxy (Proxy))
import Data.Swagger hiding (title)
import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow)
import GHC.Generics (Generic)
import Prelude hiding (id)

data Essay = Essay
    { id :: !Int
    , title :: !(Maybe Text)
    , grade :: !Int
    , task :: !Text
    , model :: !Text
    }
    deriving (Show, Read, Generic)

instance FromJSON Essay
instance ToJSON Essay where
    toJSON :: Essay -> Value
    toJSON e =
        object
            [ "id" .= id e
            , "title" .= title e
            , "grade" .= grade e
            , "task" .= task e
            ]

instance FromRow Essay
instance ToSchema Essay where
    declareNamedSchema _ = do
        intSchema <- declareSchemaRef (Proxy :: Proxy Int)
        textSchema <- declareSchemaRef (Proxy :: Proxy Text)

        return $
            NamedSchema (Just "Essay") $
                mempty
                    & properties
                        .~ [ ("id", intSchema)
                           , ("title", textSchema)
                           , ("grade", intSchema)
                           , ("task", textSchema)
                           ]
                    & required .~ ["id", "grade", "task"]
