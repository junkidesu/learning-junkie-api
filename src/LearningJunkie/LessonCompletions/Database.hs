module LearningJunkie.LessonCompletions.Database where

import Data.Int (Int32)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (runInsertReturningList))
import Database.Beam.Postgres (Postgres)
import LearningJunkie.Courses.Database.Table (CourseT (_courseId))
import LearningJunkie.Database (LearningJunkieDb (dbLessonCompletions), db)
import LearningJunkie.Database.Util (executeBeamDebug, tripleFst, tripleSnd, tripleThrd)
import LearningJunkie.LessonCompletions.Database.Table (LessonCompletion, LessonCompletionT (LessonCompletion, _lessonCompletionId, _lessonCompletionLesson, _lessonCompletionTime, _lessonCompletionUser))
import qualified LearningJunkie.LessonCompletions.LessonCompletion as LC
import LearningJunkie.Lessons.Database (LessonJoinedType, LessonReturnType, allLessonsQuery, lessonByIdQuery, toLessonType)
import LearningJunkie.Lessons.Database.Table (PrimaryKey (LessonId))
import LearningJunkie.Users.Database (UserJoinedType, UserReturnType, allUsersQuery, toUserType, userByIdQuery)
import LearningJunkie.Users.Database.Table (PrimaryKey (UserId))
import LearningJunkie.Web.AppM (AppM)

type LessonCompletionExpr s = LessonCompletionT (QExpr Postgres s)
type LessonCompletionJoinedType s = (LessonCompletionExpr s, UserJoinedType s, LessonJoinedType s)
type LessonCompletionQ s = Q Postgres LearningJunkieDb s (LessonCompletionJoinedType s)
type LessonCompletionReturnType = (LessonCompletion, UserReturnType, LessonReturnType)

allLessonCompletionsByUserIdQ :: Int32 -> LessonCompletionQ s
allLessonCompletionsByUserIdQ userId = do
    lessonCompletion <- all_ $ dbLessonCompletions db

    foundUser@(user, _) <- userByIdQuery userId

    guard_ $ _lessonCompletionUser lessonCompletion `references_` user

    foundLesson@(lesson, _) <- allLessonsQuery

    guard_ $ _lessonCompletionLesson lessonCompletion `references_` lesson

    return (lessonCompletion, foundUser, foundLesson)

allLessonCompletionsByLessonIdQ :: Int32 -> LessonCompletionQ s
allLessonCompletionsByLessonIdQ lessonId = do
    lessonCompletion <- all_ $ dbLessonCompletions db

    foundUser@(user, _) <- allUsersQuery

    guard_ $ _lessonCompletionUser lessonCompletion `references_` user

    foundLesson@(lesson, _) <- lessonByIdQuery lessonId

    guard_ $ _lessonCompletionLesson lessonCompletion `references_` lesson

    return (lessonCompletion, foundUser, foundLesson)

lessonCompletionsByCourseIdQ :: Int32 -> LessonCompletionQ s
lessonCompletionsByCourseIdQ courseId = do
    lessonCompletion <- all_ $ dbLessonCompletions db

    foundUser@(user, _) <- allUsersQuery

    guard_ $ _lessonCompletionUser lessonCompletion `references_` user

    foundLesson@(lesson, _course@(course, _, _, _, _, _)) <- allLessonsQuery

    guard_ $ _lessonCompletionLesson lessonCompletion `references_` lesson

    guard_ $ _courseId course ==. val_ courseId

    return (lessonCompletion, foundUser, foundLesson)

insertLessonCompletionQ :: Int32 -> Int32 -> SqlInsert Postgres LessonCompletionT
insertLessonCompletionQ userId lessonId =
    insert (dbLessonCompletions db) $
        insertExpressions
            [ LessonCompletion
                default_
                (val_ $ UserId userId)
                (val_ $ LessonId lessonId)
                default_
            ]

insertLessonCompletion :: Int32 -> Int32 -> AppM LessonCompletionReturnType
insertLessonCompletion userId lessonId = executeBeamDebug $ do
    [insertedLessonCompletion] <- runInsertReturningList $ insertLessonCompletionQ userId lessonId

    [user] <- runSelectReturningList $ select $ userByIdQuery userId

    [lesson] <- runSelectReturningList $ select $ lessonByIdQuery lessonId

    return (insertedLessonCompletion, user, lesson)

selectAllLessonCompletionsByUserId :: Int32 -> AppM [LessonCompletionReturnType]
selectAllLessonCompletionsByUserId =
    executeBeamDebug
        . runSelectReturningList
        . select
        . allLessonCompletionsByUserIdQ

selectAllLessonCompletionsByLessonId :: Int32 -> AppM [LessonCompletionReturnType]
selectAllLessonCompletionsByLessonId =
    executeBeamDebug
        . runSelectReturningList
        . select
        . allLessonCompletionsByLessonIdQ

selectLessonCompletionsByCourseId :: Int32 -> AppM [LessonCompletionReturnType]
selectLessonCompletionsByCourseId =
    executeBeamDebug
        . runSelectReturningList
        . select
        . lessonCompletionsByCourseIdQ

toLessonCompletionType :: LessonCompletionReturnType -> LC.LessonCompletion
toLessonCompletionType =
    LC.LessonCompletion
        <$> _lessonCompletionId . tripleFst
        <*> toUserType . tripleSnd
        <*> toLessonType . tripleThrd
        <*> _lessonCompletionTime . tripleFst
