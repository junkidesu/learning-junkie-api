{-# LANGUAGE TypeApplications #-}

module LearningJunkie.Courses.Database where

import Data.Int (Int32)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (runInsertReturningList), MonadBeamUpdateReturning (runUpdateReturningList))
import Database.Beam.Postgres
import LearningJunkie.Chapters.Database.Table (PrimaryKey (ChapterId))
import qualified LearningJunkie.Courses.Course as Course
import qualified LearningJunkie.Courses.Course.Attributes as Attributes
import LearningJunkie.Courses.Database.Table
import LearningJunkie.Database
import LearningJunkie.Database.Util (executeBeamDebug, fromJSONB, updateIfChanged)
import LearningJunkie.Enrollments.Database.Table (EnrollmentT (_enrollmentCourse))
import LearningJunkie.Exercises.Database.Table (ExerciseT (_exerciseLesson))
import LearningJunkie.Lessons.Database.Table (LessonT (_lessonChapter))
import LearningJunkie.Universities.Database
import LearningJunkie.Universities.Database.Table (PrimaryKey (UniversityId), University, UniversityT (_universityId))
import LearningJunkie.Users.Database
import LearningJunkie.Users.Database.Table (PrimaryKey (UserId), UserT (_userId))
import LearningJunkie.Web.AppM (AppM)

type TotalLessonsNum = Int32
type TotalLessonsNumExpr s = QExpr Postgres s Int32

type TotalExercisesNum = Int32
type TotalExercisesNumExpr s = QExpr Postgres s Int32

type EnrollmentsCount = Int32
type EnrollmentsCountExpr s = QExpr Postgres s Int32

type CourseExpr s = CourseT (QExpr Postgres s)
type CourseJoinedType s =
    ( CourseExpr s
    , UniversityExpr s
    , UserJoinedType s
    , TotalLessonsNumExpr s
    , TotalExercisesNumExpr s
    , EnrollmentsCountExpr s
    )
type CourseQ s =
    Q
        Postgres
        LearningJunkieDb
        s
        (CourseJoinedType s)
type CourseReturnType =
    ( Course
    , University
    , UserReturnType
    , TotalLessonsNum
    , TotalExercisesNum
    , EnrollmentsCount
    )

allCoursesQuery :: CourseQ s
allCoursesQuery = do
    course <- all_ $ dbCourses db

    university <- allUniversitiesQuery

    guard_ (_courseUniversity course `references_` university)

    instructor@(user, _) <- allUsersQuery

    guard_ (_courseInstructor course `references_` user)

    (_, mbTotalLessonsNum) <-
        leftJoin_
            ( aggregate_
                ( \lesson ->
                    ( group_
                        (let ChapterId courseId _ = _lessonChapter lesson in courseId)
                    , as_ @Int32 $ countAll_
                    )
                )
                $ do
                    all_ (dbLessons db)
            )
            ( \(courseId, _) ->
                courseId `references_` course
            )

    (_, mbTotalExercisesNum) <-
        leftJoin_
            ( nub_
                $ aggregate_
                    ( \(_exercise, lesson) ->
                        ( group_
                            (let ChapterId courseId _ = _lessonChapter lesson in courseId)
                        , as_ @Int32 $ countAll_
                        )
                    )
                $ do
                    exercise <- all_ (dbExercises db)

                    lesson <- all_ (dbLessons db)

                    guard_ (_exerciseLesson exercise `references_` lesson)

                    return (exercise, lesson)
            )
            (\(courseId, _) -> courseId `references_` course)

    (_, mbEnrollmentsCount) <-
        leftJoin_
            ( nub_
                $ aggregate_
                    ( \r ->
                        ( group_
                            ( let courseId =
                                    _enrollmentCourse r
                               in courseId
                            )
                        , as_ @Int32 $ countAll_
                        )
                    )
                $ do
                    all_ (dbEnrollments db)
            )
            (\(courseId, _) -> courseId `references_` course)

    let totalLessonsNum = maybe_ (val_ (0 :: Int32)) id mbTotalLessonsNum
    let totalExercisesNum = maybe_ (val_ (0 :: Int32)) id mbTotalExercisesNum
    let enrollmentsCount = maybe_ (val_ (0 :: Int32)) id mbEnrollmentsCount

    return (course, university, instructor, totalLessonsNum, totalExercisesNum, enrollmentsCount)

courseByIdQuery :: Int32 -> CourseQ s
courseByIdQuery courseId =
    filter_
        ( \(course, _, _, _, _, _) ->
            _courseId course ==. val_ courseId
        )
        allCoursesQuery

coursesByUniversityIdQ :: Int32 -> CourseQ s
coursesByUniversityIdQ universityId =
    filter_
        (\(_, university, _, _, _, _) -> _universityId university ==. val_ universityId)
        allCoursesQuery

coursesByInstructorIdQ :: Int32 -> CourseQ s
coursesByInstructorIdQ instructorId =
    filter_
        ( \_r@(_, _, _instructor@(user, _), _, _, _) ->
            _userId user ==. val_ instructorId
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
                (val_ $ PgJSONB $ Attributes.completionRequirements newCourse)
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
                <> updateIfChanged _courseCompletionRequirements r (PgJSONB <$> Attributes.completionRequirements editCourse)
        )
        (\r -> _courseId r ==. val_ courseId)

deleteCourseQuery :: Int32 -> SqlDelete Postgres CourseT
deleteCourseQuery courseId = delete (dbCourses db) (\r -> _courseId r ==. val_ courseId)

selectAllCourses :: AppM [CourseReturnType]
selectAllCourses =
    executeBeamDebug
        . runSelectReturningList
        . select
        $ allCoursesQuery

selectCourseById :: Int32 -> AppM (Maybe CourseReturnType)
selectCourseById =
    executeBeamDebug
        . runSelectReturningFirst
        . select
        . courseByIdQuery

selectCoursesByUniversityId :: Int32 -> AppM [CourseReturnType]
selectCoursesByUniversityId =
    executeBeamDebug
        . runSelectReturningList
        . select
        . coursesByUniversityIdQ

selectCoursesByInstructorId :: Int32 -> AppM [CourseReturnType]
selectCoursesByInstructorId =
    executeBeamDebug
        . runSelectReturningList
        . select
        . coursesByInstructorIdQ

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

    return (course, university, instructor, 0, 0, 0)

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

    return (course, university, instructor, 0, 0, 0)

deleteCourse :: Int32 -> AppM ()
deleteCourse =
    executeBeamDebug
        . runDelete
        . deleteCourseQuery

toCourseType :: CourseReturnType -> Course.Course
toCourseType (course, university, instructor, totalLessonsNum, totalExercisesNum, enrollmentsCount) =
    Course.Course
        (_courseId course)
        (_courseTitle course)
        (_courseDescription course)
        (_courseDifficulty course)
        (_courseBanner course)
        (toUniversityType university)
        (toUserType instructor)
        (fromJSONB . _courseCompletionRequirements $ course)
        totalLessonsNum
        totalExercisesNum
        enrollmentsCount
