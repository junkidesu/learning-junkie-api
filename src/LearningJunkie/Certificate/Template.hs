{-# LANGUAGE OverloadedStrings #-}

module LearningJunkie.Certificate.Template where

import LearningJunkie.CourseCompletions.CourseCompletion (CourseCompletion (course))
import LearningJunkie.Courses.Course (Course (title, university))
import LearningJunkie.Universities.University (University (name))
import Lucid

certificateTemplate :: CourseCompletion -> Html ()
certificateTemplate courseCompletion = html_ $ do
    div_ $ do
        p_ "Learning Junkie"
        p_ . toHtml . name . university . course $ courseCompletion
    div_ $
        b_ $
            p_ "CERTIFICATE"
