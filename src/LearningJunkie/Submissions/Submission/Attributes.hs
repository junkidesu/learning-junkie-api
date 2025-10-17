{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module LearningJunkie.Submissions.Submission.Attributes where

import Conduit (Identity)
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import LearningJunkie.Attribute (Attribute)
import LearningJunkie.Submissions.Submission.Content (SubmissionContent)

newtype SubmissionAttributes f = Attributes
    { content :: Attribute f SubmissionContent
    }
    deriving (Generic)

type New = SubmissionAttributes Identity
type Edit = SubmissionAttributes Maybe

deriving instance FromJSON New
deriving instance ToJSON New
deriving instance ToSchema New
deriving instance FromJSON Edit
deriving instance ToJSON Edit
deriving instance ToSchema Edit
