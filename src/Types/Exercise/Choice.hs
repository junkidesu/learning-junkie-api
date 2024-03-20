{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Exercise.Choice (Choice (..)) where

import Control.Lens hiding (Choice, (.=))
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Char8 as BS
import Data.Data (Proxy (Proxy))
import Data.Swagger (HasProperties (properties), HasRequired (required), NamedSchema (NamedSchema), ToSchema (declareNamedSchema), declareSchemaRef)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField (toField))
import Database.PostgreSQL.Simple.TypeInfo.Static (text)
import GHC.Generics (Generic)

data Choice = A | B | C | D
    deriving (Show, Eq, Generic)

instance FromJSON Choice
instance ToJSON Choice
instance ToSchema Choice

instance ToField Choice where
    toField :: Choice -> Action
    toField A = Escape "a"
    toField B = Escape "b"
    toField C = Escape "c"
    toField D = Escape "d"

instance FromField Choice where
    fromField :: FieldParser Choice
    fromField f mdata =
        if typeOid f /= typoid text
            then returnError Incompatible f ""
            else case BS.unpack <$> mdata of
                Nothing -> returnError UnexpectedNull f ""
                Just dat -> case dat of
                    "a" -> pure A
                    "b" -> pure B
                    "c" -> pure C
                    "d" -> pure D
                    _ -> returnError ConversionFailed f ""

instance ToJSON (Choice -> Text) where
    toJSON :: (Choice -> Text) -> Value
    toJSON os = object ["A" .= os A, "B" .= os B, "C" .= os C, "D" .= os D]

instance FromJSON (Choice -> Text) where
    parseJSON :: Value -> Parser (Choice -> Text)
    parseJSON = withObject "Choice -> Text" $ \v -> do
        optionA <- v .: "A"
        optionB <- v .: "B"
        optionC <- v .: "C"
        optionD <- v .: "D"
        return $ \case
            A -> optionA
            B -> optionB
            C -> optionC
            D -> optionD

instance ToSchema (Choice -> Text) where
    declareNamedSchema _ = do
        textSchema <- declareSchemaRef (Proxy :: Proxy Text)

        return $
            NamedSchema (Just "QuizOptions") $
                mempty
                    & properties
                        .~ [ ("A", textSchema)
                           , ("B", textSchema)
                           , ("C", textSchema)
                           , ("D", textSchema)
                           ]
                    & required .~ ["A", "B", "C", "D"]
