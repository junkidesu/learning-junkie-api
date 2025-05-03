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

allLessonsQuery :: Int32 -> Int32 -> LessonQuery s
allLessonsQuery courseId chapterNumber = do
    lesson <- all_ $ dbLessons db

    let LessonId
            _
            ( ChapterId
                    (CourseId lessonCourseId)
                    lessonChapterNumber
                ) = primaryKey lesson

    guard_
        ( val_ courseId
            ==. lessonCourseId
            &&. val_ chapterNumber
            ==. lessonChapterNumber
        )

    return lesson

lessonByNumberQuery :: Int32 -> Int32 -> Int32 -> LessonQuery s
lessonByNumberQuery courseId chapterNumber lessonNumber =
    filter_ (\r -> _lessonLessonNumber r ==. val_ lessonNumber) $
        allLessonsQuery courseId chapterNumber

insertLessonQuery :: Int32 -> Int32 -> Attributes.New -> SqlInsert Postgres LessonT
insertLessonQuery courseId chapterNumber newLesson =
    insert (dbLessons db) $
        insertExpressions
            [ Lesson
                (val_ $ Attributes.number newLesson)
                (ChapterId (CourseId $ val_ courseId) (val_ chapterNumber))
                (val_ $ Attributes.title newLesson)
                (val_ $ Attributes.description newLesson)
                (val_ $ PgJSONB $ Attributes.components newLesson)
            ]

updateLessonQuery :: Int32 -> Int32 -> Int32 -> Attributes.Edit -> SqlUpdate Postgres LessonT
updateLessonQuery courseId chapterNumber lessonNumber editLesson =
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
        (\r -> primaryKey r ==. LessonId (val_ lessonNumber) (ChapterId (CourseId $ val_ courseId) (val_ chapterNumber)))

selectAllLessons :: Int32 -> Int32 -> AppM [Lesson]
selectAllLessons courseId chapterNumber =
    executeBeamDebug
        . runSelectReturningList
        . select
        $ allLessonsQuery courseId chapterNumber

selectLessonByNumber :: Int32 -> Int32 -> Int32 -> AppM (Maybe Lesson)
selectLessonByNumber courseId chapterNumber lessonNumber =
    executeBeamDebug
        . runSelectReturningFirst
        . select
        $ lessonByNumberQuery courseId chapterNumber lessonNumber

insertLesson :: Int32 -> Int32 -> Attributes.New -> AppM Lesson
insertLesson courseId chapterNumber newLesson = executeBeamDebug $ do
    [lesson] <-
        runInsertReturningList $
            insertLessonQuery
                courseId
                chapterNumber
                newLesson

    return lesson

updateLesson :: Int32 -> Int32 -> Int32 -> Attributes.Edit -> AppM Lesson
updateLesson courseId chapterNumber lessonNumber editLesson = executeBeamDebug $ do
    [lesson] <-
        runUpdateReturningList $
            updateLessonQuery
                courseId
                chapterNumber
                lessonNumber
                editLesson

    return lesson

toLessonType :: Lesson -> Lesson.Lesson
toLessonType lesson =
    let PgJSONB components = _lessonComponents lesson
     in Lesson.Lesson
            (_lessonLessonNumber lesson)
            (_lessonTitle lesson)
            (_lessonDescription lesson)
            components
