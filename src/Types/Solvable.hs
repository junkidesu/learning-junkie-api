{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Solvable (Solvable (..)) where

import Types.Solution.ExerciseCheck (ExerciseCheck)

class Solvable exercise solution where
    checkSolution :: exercise -> solution -> ExerciseCheck
