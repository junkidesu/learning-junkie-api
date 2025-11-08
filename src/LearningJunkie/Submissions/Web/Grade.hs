{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Submissions.Web.Grade where

import Control.Monad (when)
import Data.Int (Int32)
import LearningJunkie.Submissions.Database (modifySubmissionGrade, selectSubmissionById, toSubmissionType)
import LearningJunkie.Submissions.Submission (Submission)
import LearningJunkie.Submissions.Submission.ManualGrade (ManualGrade)
import LearningJunkie.Users.Database.Table (PrimaryKey (UserId), UserT (_userId))
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
handler (Authenticated authUser) submissionId manualGrade = do
    mbSubmission <- selectSubmissionById submissionId

    case mbSubmission of
        Nothing -> throwError err404
        Just
            _submission@( _
                            , _
                            , _exercise@( _
                                            , _lesson@( _
                                                        , _course@( _
                                                                    , _
                                                                    , _instructor@(user, _)
                                                                    , _
                                                                    , _
                                                                    , _
                                                                    )
                                                        )
                                            )
                            ) -> do
                -- we first check if the teacher is allowed to change the submission
                let
                    userId :: Int32
                    userId = _userId user

                -- Cannot grade if the instructor is not from the course
                when (Auth.id authUser /= userId) $ throwError err401

                -- first let's check the state of the exercise. If it is success, cannot change manually.
                toSubmissionType <$> modifySubmissionGrade submissionId manualGrade
handler _ _ _ = throwError err401
