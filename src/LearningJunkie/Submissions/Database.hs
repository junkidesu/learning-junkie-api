module LearningJunkie.Submissions.Database where

import Control.Exception (throw)
import Data.Int (Int32)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (runInsertReturningList), MonadBeamUpdateReturning (runUpdateReturningList))
import Database.Beam.Postgres (PgJSONB (PgJSONB), Postgres)
import LearningJunkie.Database (LearningJunkieDb (dbSubmissions), db)
import LearningJunkie.Database.Util (executeBeamDebug, tripleFst, tripleSnd, tripleThrd, updateIfChanged)
import LearningJunkie.Exercises.Database (ExerciseJoinedType, ExerciseReturnType, allExercisesQuery, exerciseByIdQuery, toExerciseType)
import LearningJunkie.Exercises.Database.Table (ExerciseT (_exerciseId), PrimaryKey (ExerciseId))
import LearningJunkie.Submissions.Database.Table
import qualified LearningJunkie.Submissions.Submission as Submission
import qualified LearningJunkie.Submissions.Submission.Attributes as Attributes
import LearningJunkie.Submissions.Submission.ManualGrade (ManualGrade (grade, state))
import LearningJunkie.Submissions.Submission.State (SubmissionState (Pending))
import LearningJunkie.Users.Database (UserJoinedType, UserReturnType, allUsersQuery, toUserType, userByIdQuery)
import LearningJunkie.Users.Database.Table (PrimaryKey (UserId), UserT (_userId))
import LearningJunkie.Web.AppM (AppM)

type SubmissionExpr s = SubmissionT (QExpr Postgres s)
type SubmissionJoinedType s = (SubmissionExpr s, UserJoinedType s, ExerciseJoinedType s)
type SubmissionQ s = Q Postgres LearningJunkieDb s (SubmissionJoinedType s)
type SubmissionReturnType = (Submission, UserReturnType, ExerciseReturnType)

allSubmissionsQ :: SubmissionQ s
allSubmissionsQ = do
    submission <- all_ $ dbSubmissions db

    foundUser@(user, _) <- allUsersQuery

    guard_ (_submissionUser submission `references_` user)

    foundExercise@(exercise, _) <- allExercisesQuery

    guard_ (_submissionExercise submission `references_` exercise)

    return (submission, foundUser, foundExercise)

submissionByIdQ :: Int32 -> SubmissionQ s
submissionByIdQ submissionId =
    filter_
        (\(s, _, _) -> _submissionId s ==. val_ submissionId)
        allSubmissionsQ

submissionsByExerciseIdQ :: Int32 -> SubmissionQ s
submissionsByExerciseIdQ exerciseId =
    filter_
        (\(_, _, (e, _)) -> _exerciseId e ==. val_ exerciseId)
        allSubmissionsQ

submissionsByUserIdQ :: Int32 -> SubmissionQ s
submissionsByUserIdQ userId =
    filter_
        (\(_, (u, _), _) -> _userId u ==. val_ userId)
        allSubmissionsQ

selectAllSubmissions :: AppM [SubmissionReturnType]
selectAllSubmissions =
    executeBeamDebug
        . runSelectReturningList
        . select
        $ allSubmissionsQ

selectSubmissionById :: Int32 -> AppM (Maybe SubmissionReturnType)
selectSubmissionById =
    executeBeamDebug
        . runSelectReturningFirst
        . select
        . submissionByIdQ

selectSubmissionsByExerciseId :: Int32 -> AppM [SubmissionReturnType]
selectSubmissionsByExerciseId =
    executeBeamDebug
        . runSelectReturningList
        . select
        . submissionsByExerciseIdQ

selectSubmissionsByUserId :: Int32 -> AppM [SubmissionReturnType]
selectSubmissionsByUserId =
    executeBeamDebug
        . runSelectReturningList
        . select
        . submissionsByUserIdQ

insertSubmissionQ :: Int32 -> Int32 -> Attributes.New -> SubmissionState -> Maybe Int32 -> SqlInsert Postgres SubmissionT
insertSubmissionQ userId exerciseId newSubmission state mbGrade =
    insert (dbSubmissions db) $
        insertExpressions
            [ Submission
                default_
                (UserId $ val_ userId)
                (ExerciseId $ val_ exerciseId)
                (val_ $ PgJSONB $ Attributes.content newSubmission)
                (val_ state)
                (val_ mbGrade)
            ]

upgradeSubmissionGradeQ :: Int32 -> ManualGrade -> SqlUpdate Postgres SubmissionT
upgradeSubmissionGradeQ submissionId manualGrade =
    update
        (dbSubmissions db)
        ( \r ->
            updateIfChanged _submissionGrade r (Just $ Just $ grade manualGrade)
                <> updateIfChanged _submissionState r (Just $ state manualGrade)
        )
        (\r -> _submissionId r ==. val_ submissionId)

insertSubmission :: Int32 -> Int32 -> Attributes.New -> SubmissionState -> Maybe Int32 -> AppM SubmissionReturnType
insertSubmission userId exerciseId newSubmission state mbGrade = executeBeamDebug $ do
    [insertedSubmission] <-
        runInsertReturningList
            ( insertSubmissionQ
                userId
                exerciseId
                newSubmission
                state
                mbGrade
            )

    Just user <- runSelectReturningFirst $ select $ userByIdQuery userId

    Just exercise <- runSelectReturningFirst $ select $ exerciseByIdQuery exerciseId

    return (insertedSubmission, user, exercise)

modifySubmissionGrade :: Int32 -> ManualGrade -> AppM SubmissionReturnType
modifySubmissionGrade submissionId manualGrade = executeBeamDebug $ do
    [updatedSubmission] <- runUpdateReturningList (upgradeSubmissionGradeQ submissionId manualGrade)

    let
        userId :: Int32
        UserId userId = _submissionUser updatedSubmission

        exerciseId :: Int32
        ExerciseId exerciseId = _submissionExercise updatedSubmission

    Just user <- runSelectReturningFirst $ select $ userByIdQuery userId

    Just exercise <- runSelectReturningFirst $ select $ exerciseByIdQuery exerciseId

    return (updatedSubmission, user, exercise)

toSubmissionType :: SubmissionReturnType -> Submission.Submission
toSubmissionType =
    Submission.Submission
        <$> _submissionId . tripleFst
        <*> toUserType . tripleSnd
        <*> toExerciseType . tripleThrd
        <*> fromJSONB . _submissionContent . tripleFst
        <*> _submissionState . tripleFst
        <*> _submissionGrade . tripleFst
  where
    fromJSONB (PgJSONB a) = a
