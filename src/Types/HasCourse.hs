{-# LANGUAGE InstanceSigs #-}

module Types.HasCourse (HasCourse (..)) where

import Types.Course (Course)
import Types.Exercise.Essay (Essay)
import Types.Exercise.Question (Question)
import Types.Exercise.Quiz (Quiz)

class HasCourse e where
    course :: e -> Course

instance HasCourse Question where
    course :: Question -> Course
    course = course

instance HasCourse Essay where
    course :: Essay -> Course
    course = course

instance HasCourse Quiz where
    course :: Quiz -> Course
    course = course
