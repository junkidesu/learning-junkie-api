{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Exercises.Web.Submissions.All where

import Control.Lens.Internal.CTypes (Int32)
import LearningJunkie.Submissions.Database (selectSubmissionsByExerciseId, toSubmissionType)
import LearningJunkie.Submissions.Submission (Submission)
import LearningJunkie.Web.AppM (AppM)
import Servant

type API = Summary "See all submisions to an exercise by ID" :> Get '[JSON] [Submission]

handler :: Int32 -> AppM [Submission]
handler exerciseId =
    map toSubmissionType <$> selectSubmissionsByExerciseId exerciseId
