module LearningJunkie.Courses.Database where

import Data.Int (Int32)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (runInsertReturningList))
import Database.Beam.Postgres
import qualified LearningJunkie.Courses.Course as Course
import qualified LearningJunkie.Courses.Course.Attributes as Attributes
import LearningJunkie.Courses.Database.Table
import LearningJunkie.Database
import LearningJunkie.Database.Util (executeBeamDebug)
import LearningJunkie.Universities.Database
import LearningJunkie.Universities.Database.Table (PrimaryKey (UniversityId), University)
import LearningJunkie.Users.Database (UserDBType, allUsersQuery, toUserType, userByIdQuery)
import LearningJunkie.Users.Database.Table (PrimaryKey (UserId), User)
import LearningJunkie.Web.AppM (AppM)

type CourseDBType s = CourseT (QExpr Postgres s)
type CourseQuery s =
    Q
        Postgres
        LearningJunkieDb
        s
        ( CourseDBType s
        , UniversityDBType s
        , UserDBType s
        )

allCoursesQuery :: CourseQuery s
allCoursesQuery = do
    course <- all_ $ dbCourses db

    university <- allUniversitiesQuery

    guard_ (_courseUniversity course `references_` university)

    instructor@(user, _) <- allUsersQuery

    guard_ (_courseInstructor course `references_` user)

    return (course, university, instructor)

courseByIdQuery :: Int32 -> CourseQuery s
courseByIdQuery courseId =
    filter_
        ( \(course, _, _) ->
            _courseId course ==. val_ courseId
        )
        allCoursesQuery

insertCourseQuery :: Attributes.New -> SqlInsert Postgres CourseT
insertCourseQuery newCourse = do
    insert (dbCourses db) $
        insertExpressions
            [ Course
                default_
                (val_ $ Attributes.title newCourse)
                (val_ $ Attributes.description newCourse)
                (val_ $ Attributes.difficulty newCourse)
                (val_ $ Attributes.banner newCourse)
                (val_ $ UniversityId $ Attributes.university newCourse)
                (val_ $ UserId $ Attributes.instructor newCourse)
            ]

selectAllCourses :: AppM [(Course, University, (User, Maybe University))]
selectAllCourses =
    executeBeamDebug $
        runSelectReturningList $
            select $
                allCoursesQuery

selectCourseById :: Int32 -> AppM (Maybe (Course, University, (User, Maybe University)))
selectCourseById =
    executeBeamDebug
        . runSelectReturningFirst
        . select
        . courseByIdQuery

insertCourse :: Attributes.New -> AppM (Course, University, (User, Maybe University))
insertCourse newCourse = executeBeamDebug $ do
    [course] <- runInsertReturningList $ insertCourseQuery newCourse

    [university] <- runSelectReturningList $ select $ universityByIdQuery (Attributes.university newCourse)

    [instructor] <- runSelectReturningList $ select $ userByIdQuery (Attributes.instructor newCourse)

    return (course, university, instructor)

toCourseType :: (Course, University, (User, Maybe University)) -> Course.Course
toCourseType =
    Course.Course
        <$> (_courseId . tripleFst)
        <*> (_courseTitle . tripleFst)
        <*> (_courseDescription . tripleFst)
        <*> (_courseDifficulty . tripleFst)
        <*> (_courseBanner . tripleFst)
        <*> (toUniversityType . tripleSnd)
        <*> (toUserType . tripleThrd)
  where
    tripleFst (x, _, _) = x
    tripleSnd (_, x, _) = x
    tripleThrd (_, _, x) = x
