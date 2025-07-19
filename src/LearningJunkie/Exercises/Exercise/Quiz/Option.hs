{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module LearningJunkie.Exercises.Exercise.Quiz.Option where

import Control.Lens hiding ((.=))
import Data.Aeson (
        FromJSON (parseJSON),
        KeyValue ((.=)),
        ToJSON (toJSON),
        object,
        withObject,
        (.:),
 )
import Data.Aeson.Types (Parser)
import Data.List (intercalate)
import Data.OpenApi
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text, unpack)
import GHC.Generics (Generic)

data Option = A | B | C | D deriving (Show, Generic)

instance FromJSON Option
instance ToJSON Option
instance ToSchema Option

instance Show (Option -> Text) where
        show :: (Option -> Text) -> String
        show os =
                let quizOptions = [A, B, C, D]
                 in intercalate
                        "\n"
                        (map (\o -> show o ++ ": " ++ unpack (os o)) quizOptions)

instance FromJSON (Option -> Text) where
        parseJSON = withObject "Options" $ \v -> do
                optionA <- (v .: "A") :: Parser Text
                optionB <- (v .: "B") :: Parser Text
                optionC <- (v .: "C") :: Parser Text
                optionD <- (v .: "D") :: Parser Text

                let
                        func :: Option -> Text
                        func A = optionA
                        func B = optionB
                        func C = optionC
                        func D = optionD

                return func

instance ToJSON (Option -> Text) where
        toJSON opts =
                object
                        [ "A" .= opts A
                        , "B" .= opts B
                        , "C" .= opts C
                        , "D" .= opts D
                        ]

instance ToSchema (Option -> Text) where
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
                                        & required
                                                .~ ["A", "B", "C", "D"]
