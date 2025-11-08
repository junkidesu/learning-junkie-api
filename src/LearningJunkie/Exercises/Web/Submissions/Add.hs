{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Exercises.Web.Submissions.Add where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Int (Int32)
import LearningJunkie.Courses.Database.Table (CourseT (_courseId))
import LearningJunkie.Enrollments.Database (checkEnrollment)
import LearningJunkie.Exercises.Database (selectExerciseById, toExerciseType)
import LearningJunkie.Exercises.Grading (GradingResult (Result), autoGradeExercise)
import LearningJunkie.Submissions.Database (insertSubmission, toSubmissionType)
import LearningJunkie.Submissions.Submission (Submission)
import qualified LearningJunkie.Submissions.Submission.Attributes as Attributes
import LearningJunkie.Web.AppM (AppM)
import qualified LearningJunkie.Web.Auth.User as Auth
import LearningJunkie.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated), ThrowAll (throwAll))

type API =
    Summary "Post a submission to an exercise by ID"
        :> JWTAuth
        :> ReqBody '[JSON] Attributes.New
        :> PostCreated '[JSON] Submission

handler :: Int32 -> AuthResult Auth.AuthUser -> Attributes.New -> AppM Submission
handler exerciseId (Authenticated authUser) newSubmission = do
    mbExercise <- selectExerciseById exerciseId

    case mbExercise of
        Nothing -> throwError err404
        Just exercise@(_, _lesson@(_, (course, _, _, _, _))) -> do
            let
                courseId :: Int32
                courseId = _courseId course

            isEnrolled <- checkEnrollment (Auth.id authUser) courseId

            if isEnrolled
                then do
                    _result@(Result state mbGrade mbComment) <-
                        autoGradeExercise
                            newSubmission
                            (toExerciseType exercise)

                    toSubmissionType
                        <$> insertSubmission
                            (Auth.id authUser)
                            exerciseId
                            newSubmission
                            state
                            mbGrade
                            mbComment
                else throwError err401{errBody = "Not enrolled in the course"}
handler _ _ _ = throwAll err401
