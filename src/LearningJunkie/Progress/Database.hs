{-# LANGUAGE TypeApplications #-}

module LearningJunkie.Progress.Database where

import Data.Int (Int32)
import Database.Beam
import Database.Beam.Postgres (Postgres)
import LearningJunkie.Courses.Database.Table (CourseT (_courseId))
import LearningJunkie.Database (LearningJunkieDb)
import LearningJunkie.Database.Util (executeBeamDebug)
import LearningJunkie.Enrollments.Database (EnrollmentJoinedType, EnrollmentReturnType, allEnrollmentsQ, enrollmentsByUserIdQ, toEnrollmentType)
import LearningJunkie.LessonCompletions.Database (allLessonCompletionsByUserIdQ, allLessonCompletionsQ)
import qualified LearningJunkie.Progress.Progress as Progress
import LearningJunkie.Submissions.Database (uniqueSubmissionsByUserIdQ)
import LearningJunkie.Web.AppM (AppM)

type CompletedLessonsNum = Int32
type CompletedLessonsNumExpr s = QGenExpr QValueContext Postgres s Int32

type CompletedExercisesNum = Int32
type CompletedExercisesNumExpr s = QGenExpr QValueContext Postgres s Int32

type ProgressQ s =
    Q
        Postgres
        LearningJunkieDb
        s
        (ProgressJoinedType s)

type ProgressJoinedType s =
    ( EnrollmentJoinedType s
    , CompletedLessonsNumExpr s
    , CompletedExercisesNumExpr s
    )

type ProgressReturnType =
    ( EnrollmentReturnType
    , CompletedLessonsNum
    , CompletedExercisesNum
    )

progressByUserIdQ :: Int32 -> ProgressQ s
progressByUserIdQ userId = do
    foundEnrollment@(_enrollment, _, (course, _, _, _, _, _)) <-
        enrollmentsByUserIdQ userId

    let
        courseId = _courseId course

    (_, mbCompletedLessonsNum) <-
        leftJoin_
            ( subselect_
                $ aggregate_
                    ( \(_, _, (_, _chapter@(_, _course@(lessonCourse, _, _, _, _, _)))) ->
                        ( group_ (_courseId lessonCourse)
                        , as_ @Int32 $ countAll_
                        )
                    )
                $ do
                    allLessonCompletionsByUserIdQ userId
            )
            (\(lessonCourseId, _) -> lessonCourseId ==. courseId)
    let
        completedLessonsNum = maybe_ (val_ (0 :: Int32)) id mbCompletedLessonsNum

    (_, mbCompletedExercisesNum) <-
        leftJoin_
            ( subselect_
                $ aggregate_
                    ( \(_, _, _exercise@(_, _lesson@(_, _chapter@(_, _course@(exerciseCourse, _, _, _, _, _))))) ->
                        ( group_ (_courseId exerciseCourse)
                        , as_ @Int32 $ countAll_
                        )
                    )
                $ do
                    uniqueSubmissionsByUserIdQ userId
            )
            (\(exerciseCourseId, _) -> exerciseCourseId ==. courseId)

    let
        completedExercisesNum = maybe_ (val_ (0 :: Int32)) id mbCompletedExercisesNum

    return (foundEnrollment, completedLessonsNum, completedExercisesNum)

progressByUserAndCourseIdQ :: Int32 -> Int32 -> ProgressQ s
progressByUserAndCourseIdQ userId courseId =
    filter_
        ( \((_, _, _course@(course, _, _, _, _, _)), _, _) ->
            _courseId course ==. val_ courseId
        )
        $ progressByUserIdQ userId

selectUserProgress :: Int32 -> AppM [ProgressReturnType]
selectUserProgress =
    executeBeamDebug
        . runSelectReturningList
        . select
        . progressByUserIdQ

selectProgressByUserAndCourseId :: Int32 -> Int32 -> AppM (Maybe ProgressReturnType)
selectProgressByUserAndCourseId userId courseId = do
    mbProgress <- executeBeamDebug $ do
        runSelectReturningFirst $
            select $
                progressByUserAndCourseIdQ userId courseId

    case mbProgress of
        Nothing -> return Nothing
        Just progress -> return $ Just progress

toProgressType :: ProgressReturnType -> Progress.Progress
toProgressType (enrollment, completedLessonsNum, completedExercisesNum) =
    Progress.Progress
        (toEnrollmentType enrollment)
        completedLessonsNum
        completedExercisesNum
