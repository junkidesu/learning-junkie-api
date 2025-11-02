module LearningJunkie.CourseCompletions.Database where

import Data.Int (Int32)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (runInsertReturningList))
import Database.Beam.Postgres (Postgres)
import qualified LearningJunkie.CourseCompletions.CourseCompletion as CC
import LearningJunkie.CourseCompletions.Database.Table (
    CourseCompletion,
    CourseCompletionT (CourseCompletion, _courseCompletionCourse, _courseCompletionId, _courseCompletionTime, _courseCompletionUser),
 )
import LearningJunkie.Courses.Database (CourseJoinedType, CourseReturnType, allCoursesQuery, courseByIdQuery, toCourseType)
import LearningJunkie.Courses.Database.Table (CourseT (_courseId), PrimaryKey (CourseId))
import LearningJunkie.Database (LearningJunkieDb (dbCourseCompletions), db)
import LearningJunkie.Database.Util (executeBeamDebug)
import LearningJunkie.Users.Database (UserJoinedType, UserReturnType, allUsersQuery, toUserType, userByIdQuery)
import LearningJunkie.Users.Database.Table (PrimaryKey (UserId), UserT (_userId))
import LearningJunkie.Web.AppM (AppM)

type CourseCompletionExpr s = CourseCompletionT (QExpr Postgres s)
type CourseCompletionJoinedType s = (CourseCompletionExpr s, UserJoinedType s, CourseJoinedType s)
type CourseCompletionQ s = Q Postgres LearningJunkieDb s (CourseCompletionJoinedType s)
type CourseCompletionReturnType = (CourseCompletion, UserReturnType, CourseReturnType)

allCourseCompletionsQ :: CourseCompletionQ s
allCourseCompletionsQ = do
    courseCompletion <- all_ $ dbCourseCompletions db

    foundUser@(user, _) <- allUsersQuery

    guard_ $ _courseCompletionUser courseCompletion `references_` user

    foundCourse@(course, _, _, _, _) <- allCoursesQuery

    guard_ $ _courseCompletionCourse courseCompletion `references_` course

    return (courseCompletion, foundUser, foundCourse)

courseCompletionsByUserIdQ :: Int32 -> CourseCompletionQ s
courseCompletionsByUserIdQ userId =
    filter_ (\(_, _user@(user, _), _) -> _userId user ==. val_ userId) allCourseCompletionsQ

courseCompletionsByCourseIdQ :: Int32 -> CourseCompletionQ s
courseCompletionsByCourseIdQ courseId =
    filter_ (\(_, _, _course@(course, _, _, _, _)) -> _courseId course ==. val_ courseId) allCourseCompletionsQ

insertCourseCompletionQ :: Int32 -> Int32 -> SqlInsert Postgres CourseCompletionT
insertCourseCompletionQ userId courseId =
    insert (dbCourseCompletions db) $
        insertExpressions
            [ CourseCompletion
                default_
                (val_ $ UserId userId)
                (val_ $ CourseId courseId)
                default_
            ]

selectCourseCompletionsByUserId :: Int32 -> AppM [CourseCompletionReturnType]
selectCourseCompletionsByUserId =
    executeBeamDebug
        . runSelectReturningList
        . select
        . courseCompletionsByUserIdQ

insertCourseCompletion :: Int32 -> Int32 -> AppM CourseCompletionReturnType
insertCourseCompletion userId courseId = executeBeamDebug $ do
    [courseCompletion] <-
        runInsertReturningList $
            insertCourseCompletionQ userId courseId

    Just user <- runSelectReturningFirst $ select $ userByIdQuery userId

    Just course <- runSelectReturningFirst $ select $ courseByIdQuery courseId

    return (courseCompletion, user, course)

toCourseCompletionType :: CourseCompletionReturnType -> CC.CourseCompletion
toCourseCompletionType (completion, user, course) =
    CC.CourseCompletion
        (_courseCompletionId completion)
        (toUserType user)
        (toCourseType course)
        (_courseCompletionTime completion)
