{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LearningJunkie.Exercises.Grading where

import Data.Int (Int32)
import qualified LearningJunkie.Exercises.Exercise as E
import qualified LearningJunkie.Exercises.Exercise.Content as EC
import qualified LearningJunkie.Submissions.Submission as S
import qualified LearningJunkie.Submissions.Submission.Content as SC
import LearningJunkie.Submissions.Submission.State (SubmissionState (Failure, Success))

data GradingResult = Result SubmissionState Int32
  deriving (Eq, Show, Read)

gradeExercise :: S.Submission -> E.Exercise -> GradingResult
gradeExercise submission exercise =
  helper (S.content submission) (error "No content has been provided!")
 where
  helper :: SC.SubmissionContent -> EC.Content -> GradingResult
  helper (SC.TypeAnswer submitted) (EC.TypeAnswer _ correct) =
    if submitted == correct
      then Result Success (E.maxGrade exercise)
      else Result Failure 0
  helper (SC.TrueFalse submitted) (EC.TrueFalse _ correct) =
    if submitted == correct
      then Result Success (E.maxGrade exercise)
      else Result Failure 0
  helper (SC.QuizAnswer submitted) (EC.Quiz _ _ correct) =
    if submitted == correct
      then Result Success (E.maxGrade exercise)
      else Result Failure 0
  helper _ _ = Result Failure 0
