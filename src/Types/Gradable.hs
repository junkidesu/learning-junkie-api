{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Gradable (Gradable (..), ExerciseCheck (..)) where

import Data.Text (Text)
import Types.Exercise.Question (Question)

data ExerciseCheck = ExerciseSuccess | ExerciseFailure | ExercisePending

class Gradable a b where
    solve :: a -> b -> ExerciseCheck

instance Gradable Question Text where
    solve :: Question -> Text -> ExerciseCheck
    solve = undefined
