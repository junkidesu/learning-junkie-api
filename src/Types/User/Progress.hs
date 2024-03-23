module Types.User.Progress where

import Types.Course (Course)

data Progress = Progress
    { course :: !Course
    , solvedExercises :: !Int
    , totalExercises :: !Int
    }
