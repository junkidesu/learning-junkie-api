{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Types.Exercise.Quiz (Quiz (..)) where

import Data.Aeson
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromRow
import GHC.Generics (Generic)
import Types.Exercise.Choice

data Quiz = Quiz
    { id :: !Int
    , title :: !(Maybe Text)
    , grade :: !Int
    , question :: !Text
    , options :: Choice -> Text
    , correct :: Choice
    }
    deriving (Generic)

instance FromJSON Quiz
instance ToJSON Quiz
instance ToSchema Quiz
instance FromRow Quiz where
    fromRow :: RowParser Quiz
    fromRow =
        Quiz
            <$> field
            <*> field
            <*> field
            <*> field
            <*> ( do
                    optionA <- field :: RowParser Text
                    optionB <- field :: RowParser Text
                    optionC <- field :: RowParser Text
                    optionD <- field :: RowParser Text
                    return $ \case
                        A -> optionA
                        B -> optionB
                        C -> optionC
                        D -> optionD
                )
            <*> field
