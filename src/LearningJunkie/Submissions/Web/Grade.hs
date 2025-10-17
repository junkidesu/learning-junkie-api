{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Submissions.Web.Grade where

import Data.Int (Int32)
import LearningJunkie.Submissions.Database (modifySubmissionGrade, selectSubmissionById, toSubmissionType)
import LearningJunkie.Submissions.Submission (Submission)
import LearningJunkie.Submissions.Submission.ManualGrade (ManualGrade)
import LearningJunkie.Web.AppM (AppM)
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type API =
    Summary "Grade a submission by ID"
        :> JWTAuth
        :> Capture' '[Required, Description "ID of the submission"] "id" Int32
        :> "grade"
        :> ReqBody '[JSON] ManualGrade
        :> PostCreated '[JSON] Submission

handler :: AuthResult Auth.AuthUser -> Int32 -> ManualGrade -> AppM Submission
handler (Authenticated _authUser) submissionId manualGrade = do
    mbSubmission <- selectSubmissionById submissionId

    case mbSubmission of
        Nothing -> throwError err404
        Just _submission -> do
            toSubmissionType <$> modifySubmissionGrade submissionId manualGrade
handler _ _ _ = throwError err401
