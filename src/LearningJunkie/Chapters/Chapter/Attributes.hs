{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module LearningJunkie.Chapters.Chapter.Attributes where

import Data.Aeson (FromJSON)
import Data.Functor.Identity
import Data.Int (Int32)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import LearningJunkie.Attribute (Attribute)
import LearningJunkie.Chapters.Database.Table (ChapterT (_chapterChapterNumber))

data ChapterAttributes f = ChapterAttributes
    { chapterNumber :: Attribute f Int32
    , title :: Attribute f Text
    , description :: Attribute f Text
    , banner :: Attribute f (Maybe Text)
    }

type New = ChapterAttributes Identity
type Edit = ChapterAttributes Maybe

deriving instance Show New
deriving instance Eq New
deriving instance Generic New
deriving instance FromJSON New
deriving instance ToSchema New

deriving instance Show Edit
deriving instance Eq Edit
deriving instance Generic Edit
deriving instance FromJSON Edit
deriving instance ToSchema Edit
