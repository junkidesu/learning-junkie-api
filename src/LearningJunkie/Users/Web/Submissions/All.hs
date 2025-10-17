{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.Submissions.All where

import Data.Int (Int32)
import LearningJunkie.Submissions.Database (selectSubmissionsByUserId, toSubmissionType)
import LearningJunkie.Submissions.Submission (Submission)
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
    Summary "Get all submissions of a user by ID"
        :> Get '[JSON] [Submission]

handler :: Int32 -> AppM [Submission]
handler userId =
    map toSubmissionType <$> selectSubmissionsByUserId userId
