{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LearningJunkie.Exercises.Grading where

import Data.Int (Int32)
import Data.Text (strip, toLower)
import qualified LearningJunkie.Exercises.Exercise as E
import qualified LearningJunkie.Exercises.Exercise.Content as EC
import qualified LearningJunkie.Submissions.Submission.Attributes as S
import qualified LearningJunkie.Submissions.Submission.Content as SC
import LearningJunkie.Submissions.Submission.State (SubmissionState (Failure, Pending, Success))
import LearningJunkie.Web.AppM (AppM)

data GradingResult = Result SubmissionState (Maybe Int32)
  deriving (Eq, Show, Read)

maxGrade :: E.Exercise -> GradingResult
maxGrade e = Result Success (Just $ E.maxGrade e)

autoGradeExercise :: S.New -> E.Exercise -> AppM GradingResult
autoGradeExercise submission exercise =
  return $
    helper (S.content submission) (E.content exercise)
 where
  helper :: SC.SubmissionContent -> EC.Content -> GradingResult
  helper (SC.TypeAnswer submitted) (EC.TypeAnswer _ correct) =
    if toLower (strip submitted) == toLower (strip correct)
      then Result Success (Just $ E.maxGrade exercise)
      else Result Failure (Just 0)
  helper (SC.TrueFalse submitted) (EC.TrueFalse _ correct) =
    if submitted == correct
      then Result Success (Just $ E.maxGrade exercise)
      else Result Failure (Just 0)
  helper (SC.QuizAnswer submitted) (EC.Quiz _ _ correct) =
    if submitted == correct
      then Result Success (Just $ E.maxGrade exercise)
      else Result Failure (Just 0)
  helper _ _ = Result Pending Nothing
