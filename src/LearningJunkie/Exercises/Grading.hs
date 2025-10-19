{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module LearningJunkie.Exercises.Grading where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Int (Int32)
import Data.Text (Text, strip, toLower)
import LearningJunkie.Codex (executeCode)
import qualified LearningJunkie.Codex as LJC
import qualified LearningJunkie.Exercises.Exercise as E
import qualified LearningJunkie.Exercises.Exercise.Content as EC
import qualified LearningJunkie.Submissions.Submission.Attributes as S
import qualified LearningJunkie.Submissions.Submission.Content as SC
import LearningJunkie.Submissions.Submission.State (SubmissionState (Failure, Pending, Success))
import LearningJunkie.Web.AppM (AppM)

data GradingResult = Result SubmissionState (Maybe Int32) (Maybe Text)
  deriving (Eq, Show, Read)

autoGradeExercise :: S.New -> E.Exercise -> AppM GradingResult
autoGradeExercise submission exercise =
  helper (S.content submission) (E.content exercise)
 where
  helper :: SC.SubmissionContent -> EC.Content -> AppM GradingResult
  helper (SC.TypeAnswer submitted) (EC.TypeAnswer _ correct) =
    if toLower (strip submitted) == toLower (strip correct)
      then return $ Result Success (Just $ E.maxGrade exercise) (Just "Correct")
      else return $ Result Failure (Just 0) (Just "Wrong answer")
  helper (SC.TrueFalse submitted) (EC.TrueFalse _ correct) =
    if submitted == correct
      then return $ Result Success (Just $ E.maxGrade exercise) (Just "Correct")
      else return $ Result Failure (Just 0) (Just "Wrong answer")
  helper (SC.QuizAnswer submitted) (EC.Quiz _ _ correct) =
    if submitted == correct
      then return $ Result Success (Just $ E.maxGrade exercise) (Just "Correct")
      else return $ Result Failure (Just 0) (Just "Wrong quiz option")
  helper (SC.Coding submitted) (EC.Coding _ environment correct) = do
    result <- liftIO $ executeCode environment submitted

    case result of
      LJC.Failure{} -> return $ Result Failure (Just 0) (Just $ "Code execution failure:\n" <> LJC.executionError result)
      LJC.Success{} -> do
        if strip (LJC.executionOutput result) == strip correct
          then return $ Result Success (Just $ E.maxGrade exercise) (Just "Code execution successful and the result is as expected")
          else return $ Result Failure (Just 0) (Just "The code output differs from the expected output")
  helper _ _ = return $ Result Pending Nothing Nothing
