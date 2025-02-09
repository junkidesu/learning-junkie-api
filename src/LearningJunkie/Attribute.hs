{-# LANGUAGE TypeFamilies #-}

module LearningJunkie.Attribute where

import Database.Beam (Identity)

type family Attribute f a where
    Attribute Identity a = a
    Attribute Maybe a = Maybe a
