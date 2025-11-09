module LearningJunkie.Exercises.Database where

import Data.Int (Int32)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (runInsertReturningList))
import Database.Beam.Postgres
import LearningJunkie.Database (LearningJunkieDb (dbExercises), db)
import LearningJunkie.Database.Util (executeBeamDebug)
import LearningJunkie.Exercises.Database.Table
import qualified LearningJunkie.Exercises.Exercise as Exercise
import qualified LearningJunkie.Exercises.Exercise.Attributes as Attributes
import LearningJunkie.Exercises.Exercise.Content.Response (toContentResponse)
import qualified LearningJunkie.Exercises.Exercise.Response as ExerciseResponse
import LearningJunkie.Lessons.Database (LessonJoinedType, LessonReturnType, allLessonsQ, lessonByIdQuery)
import LearningJunkie.Lessons.Database.Table (LessonT (_lessonId), PrimaryKey (LessonId))
import LearningJunkie.Web.AppM (AppM)

type ExerciseExpr s = ExerciseT (QExpr Postgres s)
type ExerciseJoinedType s = (ExerciseExpr s, LessonJoinedType s)
type ExerciseQuery s = Q Postgres LearningJunkieDb s (ExerciseJoinedType s)
type ExerciseReturnType = (Exercise, LessonReturnType)

allExercisesQuery :: ExerciseQuery s
allExercisesQuery = do
    exercise <- all_ $ dbExercises db

    foundLesson@(lesson, _) <- allLessonsQ

    guard_ (_exerciseLesson exercise ==. LessonId (_lessonId lesson))

    return (exercise, foundLesson)

exerciseByIdQuery :: Int32 -> ExerciseQuery s
exerciseByIdQuery exerciseId =
    filter_
        (\(exercise, _) -> _exerciseId exercise ==. val_ exerciseId)
        allExercisesQuery

exercisesByLessonIdQuery :: Int32 -> ExerciseQuery s
exercisesByLessonIdQuery lessonId =
    filter_
        (\(exercise, _) -> _exerciseLesson exercise ==. LessonId (val_ lessonId))
        allExercisesQuery

insertExerciseQuery :: Int32 -> Attributes.New -> SqlInsert Postgres ExerciseT
insertExerciseQuery lessonId newExercise =
    insert (dbExercises db) $
        insertExpressions
            [ Exercise
                default_
                (val_ $ Attributes.title newExercise)
                (val_ $ Attributes.description newExercise)
                (val_ $ Attributes.maxGrade newExercise)
                (val_ $ PgJSONB $ Attributes.content newExercise)
                (LessonId $ val_ lessonId)
            ]

deleteExerciseQuery :: Int32 -> SqlDelete Postgres ExerciseT
deleteExerciseQuery exerciseId = delete (dbExercises db) (\r -> _exerciseId r ==. val_ exerciseId)

selectAllExercises :: AppM [ExerciseReturnType]
selectAllExercises =
    executeBeamDebug
        . runSelectReturningList
        . select
        $ allExercisesQuery

selectExerciseById :: Int32 -> AppM (Maybe ExerciseReturnType)
selectExerciseById =
    executeBeamDebug
        . runSelectReturningFirst
        . select
        . exerciseByIdQuery

selectExercisesByLessonId :: Int32 -> AppM [ExerciseReturnType]
selectExercisesByLessonId =
    executeBeamDebug
        . runSelectReturningList
        . select
        . exercisesByLessonIdQuery

insertExercise :: Int32 -> Attributes.New -> AppM ExerciseReturnType
insertExercise lessonId newExercise = executeBeamDebug $ do
    [exercise] <-
        runInsertReturningList $
            insertExerciseQuery
                lessonId
                newExercise

    Just lesson <-
        runSelectReturningFirst $
            select $
                lessonByIdQuery lessonId

    return (exercise, lesson)

deleteExercise :: Int32 -> AppM ()
deleteExercise = executeBeamDebug . runDelete . deleteExerciseQuery

toExerciseType :: ExerciseReturnType -> Exercise.Exercise
toExerciseType =
    ( Exercise.Exercise
        <$> _exerciseId
        <*> _exerciseTitle
        <*> _exerciseDescription
        <*> _exerciseMaxGrade
        <*> fromJSONB . _exerciseContent
    )
        . fst
  where
    fromJSONB (PgJSONB a) = a

toExerciseResponseType :: ExerciseReturnType -> ExerciseResponse.ExerciseResponse
toExerciseResponseType =
    ( ExerciseResponse.ExerciseResponse
        <$> _exerciseId
        <*> _exerciseTitle
        <*> _exerciseDescription
        <*> _exerciseMaxGrade
        <*> toContentResponse . fromJSONB . _exerciseContent
    )
        . fst
  where
    fromJSONB (PgJSONB a) = a
