module LearningJunkie.Courses.Database where

import Data.Int (Int32)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (runInsertReturningList), MonadBeamUpdateReturning (runUpdateReturningList))
import Database.Beam.Postgres
import qualified LearningJunkie.Courses.Course as Course
import qualified LearningJunkie.Courses.Course.Attributes as Attributes
import LearningJunkie.Courses.Database.Table
import LearningJunkie.Database
import LearningJunkie.Database.Util (executeBeamDebug, tripleFst, tripleSnd, tripleThrd, updateIfChanged)
import LearningJunkie.Universities.Database
import LearningJunkie.Universities.Database.Table (PrimaryKey (UniversityId), University)
import LearningJunkie.Users.Database
import LearningJunkie.Users.Database.Table (PrimaryKey (UserId))
import LearningJunkie.Web.AppM (AppM)

type CourseExpr s = CourseT (QExpr Postgres s)
type CourseJoinedType s =
    ( CourseExpr s
    , UniversityExpr s
    , UserJoinedType s
    )
type CourseQ s =
    Q
        Postgres
        LearningJunkieDb
        s
        (CourseJoinedType s)
type CourseReturnType = (Course, University, UserReturnType)

allCoursesQuery :: CourseQ s
allCoursesQuery = do
    course <- all_ $ dbCourses db

    university <- allUniversitiesQuery

    guard_ (_courseUniversity course `references_` university)

    instructor@(user, _) <- allUsersQuery

    guard_ (_courseInstructor course `references_` user)

    return (course, university, instructor)

courseByIdQuery :: Int32 -> CourseQ s
courseByIdQuery courseId =
    filter_
        ( \(course, _, _) ->
            _courseId course ==. val_ courseId
        )
        allCoursesQuery

insertCourseQuery :: Attributes.New -> Int32 -> SqlInsert Postgres CourseT
insertCourseQuery newCourse universityId = do
    insert (dbCourses db) $
        insertExpressions
            [ Course
                default_
                (val_ $ Attributes.title newCourse)
                (val_ $ Attributes.description newCourse)
                (val_ $ Attributes.difficulty newCourse)
                (val_ $ Attributes.banner newCourse)
                (val_ $ UniversityId universityId)
                (val_ $ UserId $ Attributes.instructor newCourse)
            ]

updateCourseQuery :: Int32 -> Attributes.Edit -> SqlUpdate Postgres CourseT
updateCourseQuery courseId editCourse = do
    update
        (dbCourses db)
        ( \r ->
            updateIfChanged _courseTitle r (Attributes.title editCourse)
                <> updateIfChanged _courseBanner r (Attributes.banner editCourse)
                <> updateIfChanged _courseDescription r (Attributes.description editCourse)
                <> updateIfChanged _courseDifficulty r (Attributes.difficulty editCourse)
        )
        (\r -> _courseId r ==. val_ courseId)

deleteCourseQuery :: Int32 -> SqlDelete Postgres CourseT
deleteCourseQuery courseId = delete (dbCourses db) (\r -> _courseId r ==. val_ courseId)

selectAllCourses :: AppM [CourseReturnType]
selectAllCourses =
    executeBeamDebug $
        runSelectReturningList $
            select $
                allCoursesQuery

selectCourseById :: Int32 -> AppM (Maybe CourseReturnType)
selectCourseById =
    executeBeamDebug
        . runSelectReturningFirst
        . select
        . courseByIdQuery

insertCourse :: Attributes.New -> Int32 -> AppM CourseReturnType
insertCourse newCourse universityId = executeBeamDebug $ do
    [course] <- runInsertReturningList $ insertCourseQuery newCourse universityId

    let
        courseUniversityId :: Int32
        UniversityId courseUniversityId = _courseUniversity course

        instructorId :: Int32
        UserId instructorId = _courseInstructor course

    [university] <- runSelectReturningList $ select $ universityByIdQuery courseUniversityId

    [instructor] <- runSelectReturningList $ select $ userByIdQuery instructorId

    return (course, university, instructor)

updateCourse :: Int32 -> Attributes.Edit -> AppM CourseReturnType
updateCourse courseId editCourse = executeBeamDebug $ do
    [course] <- runUpdateReturningList $ updateCourseQuery courseId editCourse

    let
        universityId :: Int32
        UniversityId universityId = _courseUniversity course

        instructorId :: Int32
        UserId instructorId = _courseInstructor course

    [university] <- runSelectReturningList $ select $ universityByIdQuery universityId

    [instructor] <- runSelectReturningList $ select $ userByIdQuery instructorId

    return (course, university, instructor)

deleteCourse :: Int32 -> AppM ()
deleteCourse =
    executeBeamDebug
        . runDelete
        . deleteCourseQuery

toCourseType :: CourseReturnType -> Course.Course
toCourseType =
    Course.Course
        <$> (_courseId . tripleFst)
        <*> (_courseTitle . tripleFst)
        <*> (_courseDescription . tripleFst)
        <*> (_courseDifficulty . tripleFst)
        <*> (_courseBanner . tripleFst)
        <*> (toUniversityType . tripleSnd)
        <*> (toUserType . tripleThrd)
