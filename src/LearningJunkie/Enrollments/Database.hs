module LearningJunkie.Enrollments.Database where

import Data.Int (Int32)
import Data.Maybe (isJust)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (runInsertReturningList))
import Database.Beam.Postgres (Postgres)
import LearningJunkie.Courses.Database
import LearningJunkie.Courses.Database.Table
import LearningJunkie.Database
import LearningJunkie.Database.Util (executeBeamDebug, tripleFst, tripleSnd, tripleThrd)
import LearningJunkie.Enrollments.Database.Table
import qualified LearningJunkie.Enrollments.Enrollment as Enrollment
import LearningJunkie.Users.Database
import LearningJunkie.Users.Database.Table
import LearningJunkie.Web.AppM (AppM)

type EnrollmentExpr s = EnrollmentT (QExpr Postgres s)
type EnrollmentQ s =
    Q
        Postgres
        LearningJunkieDb
        s
        ( EnrollmentExpr s
        , UserJoinedType s
        , CourseJoinedType s
        )
type EnrollmentReturnType =
    ( Enrollment
    , UserReturnType
    , CourseReturnType
    )

courseEnrollmentsByIdQuery :: Int32 -> EnrollmentQ s
courseEnrollmentsByIdQuery courseId = do
    foundCourse@(course, _, _, _, _, _) <- courseByIdQuery courseId

    enrollment <- all_ $ dbEnrollments db

    guard_ $ _enrollmentCourse enrollment `references_` course

    foundUser@(user, _) <- allUsersQuery

    guard_ $ _enrollmentUser enrollment `references_` user

    return (enrollment, foundUser, foundCourse)

enrollmentsByUserIdQ :: Int32 -> EnrollmentQ s
enrollmentsByUserIdQ userId = do
    foundUser@(user, _) <- userByIdQuery userId

    enrollment <- all_ $ dbEnrollments db

    guard_ $ _enrollmentUser enrollment `references_` user

    foundCourse@(course, _, _, _, _, _) <- allCoursesQuery

    guard_ $ _enrollmentCourse enrollment `references_` course

    return (enrollment, foundUser, foundCourse)

checkEnrollmentQ :: Int32 -> Int32 -> EnrollmentQ s
checkEnrollmentQ courseId userId = do
    foundEnrollment@(enrollment, _, _) <- courseEnrollmentsByIdQuery courseId

    guard_ $ _enrollmentUser enrollment ==. UserId (val_ userId)

    return foundEnrollment

insertEnrollmentQuery :: Int32 -> Int32 -> SqlInsert Postgres EnrollmentT
insertEnrollmentQuery userId courseId =
    insert
        (dbEnrollments db)
        $ insertExpressions
            [ Enrollment
                (UserId $ val_ userId)
                (CourseId $ val_ courseId)
                default_
            ]

selectCourseEnrollmentsById ::
    Int32 ->
    AppM
        [EnrollmentReturnType]
selectCourseEnrollmentsById =
    executeBeamDebug
        . runSelectReturningList
        . select
        . courseEnrollmentsByIdQuery

selectEnrollmentsByUserId :: Int32 -> AppM [EnrollmentReturnType]
selectEnrollmentsByUserId =
    executeBeamDebug
        . runSelectReturningList
        . select
        . enrollmentsByUserIdQ

checkEnrollment :: Int32 -> Int32 -> AppM Bool
checkEnrollment userId courseId = executeBeamDebug $ do
    mbEnrollment <-
        runSelectReturningFirst $
            select $
                checkEnrollmentQ courseId userId

    return $ isJust mbEnrollment

insertEnrollment ::
    Int32 ->
    Int32 ->
    AppM
        EnrollmentReturnType
insertEnrollment userId courseId = executeBeamDebug $ do
    [enrollment] <- runInsertReturningList $ insertEnrollmentQuery userId courseId

    [course] <- runSelectReturningList $ select $ courseByIdQuery courseId

    [user] <- runSelectReturningList $ select $ userByIdQuery userId

    return (enrollment, user, course)

toEnrollmentType ::
    EnrollmentReturnType ->
    Enrollment.Enrollment
toEnrollmentType =
    Enrollment.Enrollment
        <$> toUserType . tripleSnd
        <*> toCourseType . tripleThrd
        <*> _enrollmentTime . tripleFst
