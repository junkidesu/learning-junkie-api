module LearningJunkie.Lessons.Database where

import Data.Int (Int32)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (runInsertReturningList), MonadBeamUpdateReturning (runUpdateReturningList))
import Database.Beam.Postgres (PgJSONB (PgJSONB), Postgres)
import LearningJunkie.Chapters.Database.Table (PrimaryKey (ChapterId))
import LearningJunkie.Courses.Database.Table (PrimaryKey (CourseId))
import LearningJunkie.Database (LearningJunkieDb (dbLessons), db)
import LearningJunkie.Database.Util (executeBeamDebug, updateIfChanged)
import LearningJunkie.Lessons.Database.Table
import qualified LearningJunkie.Lessons.Lesson as Lesson
import qualified LearningJunkie.Lessons.Lesson.Attributes as Attributes
import LearningJunkie.Web.AppM (AppM)

type LessonDBType s = LessonT (QExpr Postgres s)
type LessonQuery s =
    Q
        Postgres
        LearningJunkieDb
        s
        (LessonDBType s)

allLessonsQuery :: LessonQuery s
allLessonsQuery = all_ $ dbLessons db

lessonsByChapterQuery :: Int32 -> Int32 -> LessonQuery s
lessonsByChapterQuery courseId chapterNumber = do
    lesson <- allLessonsQuery

    let ChapterId (CourseId lessonCourseId) lessonChapterNumber = _lessonChapter lesson

    guard_ (lessonCourseId ==. val_ courseId &&. lessonChapterNumber ==. val_ chapterNumber)

    return lesson

lessonByIdQuery :: Int32 -> LessonQuery s
lessonByIdQuery lessonId =
    filter_ (\r -> _lessonId r ==. val_ lessonId) $
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

selectAllLessons :: AppM [Lesson]
selectAllLessons =
    executeBeamDebug $
        runSelectReturningList $
            select $
                allLessonsQuery

selectLessonsByChapter :: Int32 -> Int32 -> AppM [Lesson]
selectLessonsByChapter courseId chapterNumber =
    executeBeamDebug
        . runSelectReturningList
        . select
        $ lessonsByChapterQuery courseId chapterNumber

selectLessonById :: Int32 -> AppM (Maybe Lesson)
selectLessonById =
    executeBeamDebug
        . runSelectReturningFirst
        . select
        . lessonByIdQuery
insertLesson :: Int32 -> Int32 -> Attributes.New -> AppM Lesson
insertLesson courseId chapterNumber newLesson = executeBeamDebug $ do
    [lesson] <-
        runInsertReturningList $
            insertLessonQuery
                courseId
                chapterNumber
                newLesson

    return lesson

updateLesson :: Int32 -> Attributes.Edit -> AppM Lesson
updateLesson lessonId editLesson = executeBeamDebug $ do
    [lesson] <-
        runUpdateReturningList $
            updateLessonQuery
                lessonId
                editLesson

    return lesson

deleteLesson :: Int32 -> AppM ()
deleteLesson =
    executeBeamDebug
        . runDelete
        . deleteLessonQuery

toLessonType :: Lesson -> Lesson.Lesson
toLessonType lesson =
    let PgJSONB components = _lessonComponents lesson
     in Lesson.Lesson
            (_lessonId lesson)
            (_lessonLessonNumber lesson)
            (_lessonTitle lesson)
            (_lessonDescription lesson)
            components
