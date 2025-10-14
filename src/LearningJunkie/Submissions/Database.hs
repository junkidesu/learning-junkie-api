module LearningJunkie.Submissions.Database where

import Control.Exception (throw)
import Data.Int (Int32)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (runInsertReturningList))
import Database.Beam.Postgres (PgJSONB (PgJSONB), Postgres)
import LearningJunkie.Database (LearningJunkieDb (dbSubmissions), db)
import LearningJunkie.Database.Util (executeBeamDebug)
import LearningJunkie.Exercises.Database (ExerciseJoinedType, ExerciseReturnType, allExercisesQuery, exerciseByIdQuery)
import LearningJunkie.Exercises.Database.Table (ExerciseT (_exerciseId), PrimaryKey (ExerciseId))
import LearningJunkie.Submissions.Database.Table (Submission, SubmissionT (Submission, _submissionExerciseId, _submissionId, _submissionUserId))
import qualified LearningJunkie.Submissions.Submission.Attributes as Attributes
import LearningJunkie.Submissions.Submission.State (SubmissionState (Submitted))
import LearningJunkie.Users.Database (UserJoinedType, UserReturnType, allUsersQuery, userByIdQuery)
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

    guard_ (_submissionUserId submission `references_` user)

    foundExercise@(exercise, _) <- allExercisesQuery

    guard_ (_submissionExerciseId submission `references_` exercise)

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

insertSubmissionQ :: Int32 -> Int32 -> Attributes.New -> SqlInsert Postgres SubmissionT
insertSubmissionQ userId exerciseId newSubmission =
    insert (dbSubmissions db) $
        insertExpressions
            [ Submission
                default_
                (UserId $ val_ userId)
                (ExerciseId $ val_ exerciseId)
                (val_ $ PgJSONB $ Attributes.content newSubmission)
                (val_ Submitted)
                (val_ Nothing)
            ]

insertSubmission :: Int32 -> Int32 -> Attributes.New -> AppM SubmissionReturnType
insertSubmission userId exerciseId newSubmission = executeBeamDebug $ do
    [insertedSubmission] <-
        runInsertReturningList
            ( insertSubmissionQ
                userId
                exerciseId
                newSubmission
            )

    Just user <- runSelectReturningFirst $ select $ userByIdQuery userId

    Just exercise <- runSelectReturningFirst $ select $ exerciseByIdQuery exerciseId

    return (insertedSubmission, user, exercise)
