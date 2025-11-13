module LearningJunkie.Exercises.Database where

import Control.Exception (catch)
import Data.Int (Int32)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (runInsertReturningList), MonadBeamUpdateReturning (runUpdateReturningList))
import Database.Beam.Postgres
import LearningJunkie.Database (LearningJunkieDb (dbExercises), db)
import LearningJunkie.Database.Util (executeBeamDebug, updateIfChanged)
import LearningJunkie.Exercises.Database.Table
import qualified LearningJunkie.Exercises.Exercise as Exercise
import qualified LearningJunkie.Exercises.Exercise.Attributes as Attributes
import LearningJunkie.Exercises.Exercise.Content.Response (toContentResponse)
import qualified LearningJunkie.Exercises.Exercise.Response as ExerciseResponse
import LearningJunkie.Lessons.Database (LessonJoinedType, LessonReturnType, allLessonsQ, lessonByIdQuery, toLessonType)
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

updateExerciseQ :: Int32 -> Attributes.Edit -> SqlUpdate Postgres ExerciseT
updateExerciseQ exerciseId editExercise =
    update
        (dbExercises db)
        ( \r ->
            updateIfChanged _exerciseTitle r (Attributes.title editExercise)
                <> updateIfChanged _exerciseDescription r (Attributes.description editExercise)
                <> updateIfChanged _exerciseMaxGrade r (Attributes.maxGrade editExercise)
                <> updateIfChanged _exerciseContent r (PgJSONB <$> Attributes.content editExercise)
        )
        (\r -> _exerciseId r ==. val_ exerciseId)

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

updateExercise :: Int32 -> Attributes.Edit -> AppM ExerciseReturnType
updateExercise exerciseId editExercise = executeBeamDebug $ do
    [exercise] <-
        runUpdateReturningList $
            updateExerciseQ exerciseId editExercise

    let LessonId lessonId = _exerciseLesson exercise

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
    ExerciseResponse.ExerciseResponse
        <$> _exerciseId . fst
        <*> _exerciseTitle . fst
        <*> _exerciseDescription . fst
        <*> _exerciseMaxGrade . fst
        <*> toContentResponse . fromJSONB . _exerciseContent . fst
        <*> toLessonType . snd
  where
    fromJSONB (PgJSONB a) = a
