{-# LANGUAGE OverloadedStrings #-}

module LearningJunkie.Certificate.Template where

import LearningJunkie.CourseCompletions.CourseCompletion (CourseCompletion (course))
import qualified LearningJunkie.CourseCompletions.CourseCompletion as CC
import LearningJunkie.Courses.Course (Course (title, university))
import LearningJunkie.Universities.University (University (name))
import Lucid

certificateTemplate :: CC.CourseCompletion -> Html ()
certificateTemplate courseCompletion = html_ $ do
    div_ $ do
        div_ [style_ "display:flex"] $ do
            p_ "Learning Junkie"
            p_ . toHtml . name . university . course $ courseCompletion
    div_ $
        b_ $
            p_ "CERTIFICATE"
