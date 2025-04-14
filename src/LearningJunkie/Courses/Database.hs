module LearningJunkie.Courses.Database where

import Database.Beam
import Database.Beam.Postgres
import qualified LearningJunkie.Courses.Course as Course
import LearningJunkie.Courses.Database.Table
import LearningJunkie.Database
import LearningJunkie.Database.Util (executeBeamDebug)
import LearningJunkie.Universities.Database
import LearningJunkie.Universities.Database.Table (University)
import LearningJunkie.Users.Database (UserDBType, allUsersQuery, toUserType)
import LearningJunkie.Users.Database.Table (User)
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

allCourses :: AppM [(Course, University, (User, Maybe University))]
allCourses =
    executeBeamDebug $
        runSelectReturningList $
            select $
                allCoursesQuery

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
