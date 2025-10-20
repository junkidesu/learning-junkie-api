{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module LearningJunkie.Database where

import Configuration.Dotenv (defaultConfig, loadFile)
import qualified Data.ByteString.UTF8 as BSU
import Data.Pool (Pool, defaultPoolConfig, newPool, setNumStripes, withResource)
import Database.Beam
import Database.Beam.Postgres
import LearningJunkie.Chapters.Database.Table (ChapterT)
import LearningJunkie.Courses.Database.Table (CourseT)
import LearningJunkie.Enrollments.Database.Table (EnrollmentT)
import LearningJunkie.Exercises.Database.Table (ExerciseT)
import LearningJunkie.LessonCompletions.Database.Table (LessonCompletionT (_lessonCompletionId, _lessonCompletionLesson, _lessonCompletionTime, _lessonCompletionUser))
import LearningJunkie.Lessons.Database.Table (LessonT, PrimaryKey (LessonId))
import LearningJunkie.Submissions.Database.Table (SubmissionT)
import LearningJunkie.Universities.Database.Table (UniversityT)
import LearningJunkie.Users.Database.Table (PrimaryKey (UserId), UserT (_userPasswordHash))
import System.Environment (getEnv)

data LearningJunkieDb f = LearningJunkieDb
    { dbUniversities :: f (TableEntity UniversityT)
    , dbUsers :: f (TableEntity UserT)
    , dbCourses :: f (TableEntity CourseT)
    , dbEnrollments :: f (TableEntity EnrollmentT)
    , dbChapters :: f (TableEntity ChapterT)
    , dbLessons :: f (TableEntity LessonT)
    , dbExercises :: f (TableEntity ExerciseT)
    , dbSubmissions :: f (TableEntity SubmissionT)
    , dbLessonCompletions :: f (TableEntity LessonCompletionT)
    }
    deriving (Generic, Database be)

db :: DatabaseSettings db LearningJunkieDb
db =
    defaultDbSettings
        `withDbModification` dbModification
            { dbUniversities = setEntityName "universities"
            , dbUsers = setEntityName "users"
            , dbCourses = setEntityName "courses"
            , dbEnrollments = setEntityName "enrollments"
            , dbChapters = setEntityName "chapters"
            , dbLessons = setEntityName "lessons"
            , dbExercises = setEntityName "exercises"
            , dbSubmissions = setEntityName "submissions"
            , dbLessonCompletions =
                setEntityName "lesson_completions"
                    <> modifyTableFields
                        tableModification
                            { _lessonCompletionId = fieldNamed "id"
                            , _lessonCompletionUser = UserId $ fieldNamed "user__id"
                            , _lessonCompletionLesson = LessonId $ fieldNamed "lesson__id"
                            , _lessonCompletionTime = fieldNamed "time"
                            }
            }

connectToDb :: IO (Pool Connection)
connectToDb = do
    loadFile defaultConfig

    connectString <- getEnv "DATABASE_URL"

    newPool . setNumStripes (Just 2) $
        defaultPoolConfig
            (connectPostgreSQL $ BSU.fromString connectString)
            close
            60
            10
