module LearningJunkie.Lessons.Database where

import Data.Int (Int32)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (runInsertReturningList), MonadBeamUpdateReturning (runUpdateReturningList))
import Database.Beam.Postgres (PgJSONB (PgJSONB), Postgres)
import LearningJunkie.Chapters.Database.Table (PrimaryKey (ChapterId))
import LearningJunkie.Courses.Database
import LearningJunkie.Courses.Database.Table (Course, CourseT (_courseId), PrimaryKey (CourseId))
import LearningJunkie.Database (LearningJunkieDb (dbLessons), db)
import LearningJunkie.Database.Util (executeBeamDebug, tripleFst, updateIfChanged)
import LearningJunkie.Lessons.Database.Table
import qualified LearningJunkie.Lessons.Lesson as Lesson
import qualified LearningJunkie.Lessons.Lesson.Attributes as Attributes
import LearningJunkie.Web.AppM (AppM)

type LessonExpr s = LessonT (QExpr Postgres s)
type LessonJoinedType s =
    ( LessonExpr s
    , CourseJoinedType s
    )
type LessonQ s =
    Q
        Postgres
        LearningJunkieDb
        s
        (LessonJoinedType s)
type LessonReturnType = (Lesson, CourseReturnType)

allLessonsQuery :: LessonQ s
allLessonsQuery = do
    lesson <- all_ $ dbLessons db

    joinedCourse@(course, _, _, _, _) <- allCoursesQuery

    let ChapterId (CourseId courseId) _ = _lessonChapter lesson

    guard_' (courseId ==?. _courseId course)

    return (lesson, joinedCourse)

lessonsByChapterQuery :: Int32 -> Int32 -> LessonQ s
lessonsByChapterQuery courseId chapterNumber = do
    foundLesson@(lesson, _) <- allLessonsQuery

    let ChapterId (CourseId lessonCourseId) lessonChapterNumber = _lessonChapter lesson

    guard_
        ( lessonCourseId
            ==. val_ courseId
            &&. lessonChapterNumber
            ==. val_ chapterNumber
        )

    return foundLesson

lessonByIdQuery :: Int32 -> LessonQ s
lessonByIdQuery lessonId =
    filter_
        ( \r ->
            _lessonId (fst r)
                ==. val_ lessonId
        )
        allLessonsQuery

insertLessonQuery :: Int32 -> Int32 -> Attributes.New -> SqlInsert Postgres LessonT
insertLessonQuery courseId chapterNumber newLesson =
    insert (dbLessons db) $
        insertExpressions
            [ Lesson
                default_
                (val_ $ Attributes.number newLesson)
                (ChapterId (CourseId $ val_ courseId) (val_ chapterNumber))
                (val_ $ Attributes.title newLesson)
                (val_ $ Attributes.description newLesson)
                (val_ $ PgJSONB $ Attributes.components newLesson)
            ]

updateLessonQuery :: Int32 -> Attributes.Edit -> SqlUpdate Postgres LessonT
updateLessonQuery lessonId editLesson =
    update
        (dbLessons db)
        ( \r ->
            updateIfChanged
                _lessonLessonNumber
                r
                (Attributes.number editLesson)
                <> updateIfChanged
                    _lessonTitle
                    r
                    (Attributes.title editLesson)
                <> updateIfChanged
                    _lessonDescription
                    r
                    (Attributes.description editLesson)
                <> updateIfChanged
                    _lessonComponents
                    r
                    (PgJSONB <$> Attributes.components editLesson)
        )
        (\r -> _lessonId r ==. val_ lessonId)

deleteLessonQuery :: Int32 -> SqlDelete Postgres LessonT
deleteLessonQuery lessonId = delete (dbLessons db) (\r -> _lessonId r ==. val_ lessonId)

selectAllLessons :: AppM [LessonReturnType]
selectAllLessons =
    executeBeamDebug $
        runSelectReturningList $
            select $
                allLessonsQuery

selectLessonsByChapter :: Int32 -> Int32 -> AppM [LessonReturnType]
selectLessonsByChapter courseId chapterNumber =
    executeBeamDebug
        . runSelectReturningList
        . select
        $ lessonsByChapterQuery courseId chapterNumber

selectLessonById :: Int32 -> AppM (Maybe LessonReturnType)
selectLessonById =
    executeBeamDebug
        . runSelectReturningFirst
        . select
        . lessonByIdQuery

insertLesson :: Int32 -> Int32 -> Attributes.New -> AppM LessonReturnType
insertLesson courseId chapterNumber newLesson = executeBeamDebug $ do
    [lesson] <-
        runInsertReturningList $
            insertLessonQuery
                courseId
                chapterNumber
                newLesson

    Just course <- runSelectReturningFirst $ select $ courseByIdQuery courseId

    return (lesson, course)

updateLesson :: Int32 -> Attributes.Edit -> AppM LessonReturnType
updateLesson lessonId editLesson = executeBeamDebug $ do
    [lesson] <-
        runUpdateReturningList $
            updateLessonQuery
                lessonId
                editLesson

    let
        ChapterId (CourseId courseId) _ = _lessonChapter lesson
    Just course <- runSelectReturningFirst $ select $ courseByIdQuery courseId

    return (lesson, course)

deleteLesson :: Int32 -> AppM ()
deleteLesson =
    executeBeamDebug
        . runDelete
        . deleteLessonQuery

toLessonType :: LessonReturnType -> Lesson.Lesson
toLessonType =
    let fromJSONB (PgJSONB components) = components
     in ( Lesson.Lesson
            <$> _lessonId
            <*> _lessonLessonNumber
            <*> _lessonTitle
            <*> _lessonDescription
            <*> fromJSONB . _lessonComponents
        )
            . fst
