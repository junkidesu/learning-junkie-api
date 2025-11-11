module LearningJunkie.Chapters.Database where

import Data.Int (Int32)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (runInsertReturningList))
import Database.Beam.Postgres (Postgres)
import qualified LearningJunkie.Chapters.Chapter as Chapter
import qualified LearningJunkie.Chapters.Chapter.Attributes as Attributes
import LearningJunkie.Chapters.Database.Table
import LearningJunkie.Courses.Database (CourseJoinedType, CourseReturnType, allCoursesQuery, courseByIdQuery, toCourseType)
import LearningJunkie.Courses.Database.Table (PrimaryKey (CourseId))
import LearningJunkie.Database (LearningJunkieDb (dbChapters), db)
import LearningJunkie.Database.Util (executeBeamDebug)
import LearningJunkie.Web.AppM (AppM)
import Servant (NoContent)

type ChapterExpr s = ChapterT (QExpr Postgres s)
type ChapterJoinedType s = (ChapterExpr s, CourseJoinedType s)
type ChapterQ s =
    Q
        Postgres
        LearningJunkieDb
        s
        (ChapterJoinedType s)
type ChapterReturnType = (Chapter, CourseReturnType)

allChaptersQ :: ChapterQ s
allChaptersQ = do
    chapter <- all_ $ dbChapters db

    foundCourse@(course, _, _, _, _, _) <- allCoursesQuery

    guard_ (_chapterCourse chapter `references_` course)

    return (chapter, foundCourse)

allCourseChaptersQ :: Int32 -> ChapterQ s
allCourseChaptersQ courseId =
    filter_
        ( \(chapter, _) ->
            let CourseId chapterCourseId = _chapterCourse chapter
             in chapterCourseId ==. val_ courseId
        )
        allChaptersQ

chapterByCourseIdAndNumberQ :: Int32 -> Int32 -> ChapterQ s
chapterByCourseIdAndNumberQ courseId chapterNumber =
    filter_
        ( \(chapter, _) ->
            _chapterChapterNumber chapter ==. val_ chapterNumber
        )
        $ allCourseChaptersQ courseId

insertChapterQuery :: Int32 -> Attributes.New -> SqlInsert Postgres ChapterT
insertChapterQuery courseId newChapter =
    insert (dbChapters db) $
        insertExpressions
            [ Chapter
                (val_ $ Attributes.number newChapter)
                (CourseId $ val_ courseId)
                (val_ $ Attributes.title newChapter)
                (val_ $ Attributes.description newChapter)
                (val_ $ Attributes.banner newChapter)
            ]

deleteChapterQ :: Int32 -> Int32 -> SqlDelete Postgres ChapterT
deleteChapterQ courseId chapterNumber =
    delete
        (dbChapters db)
        ( \chapter ->
            let CourseId chapterCourseId = _chapterCourse chapter
             in chapterCourseId
                    ==. val_ courseId
                    &&. _chapterChapterNumber chapter
                    ==. val_ chapterNumber
        )

insertChapter :: Int32 -> Attributes.New -> AppM ChapterReturnType
insertChapter courseId newChapter = executeBeamDebug $ do
    [chapter] <-
        runInsertReturningList $
            insertChapterQuery
                courseId
                newChapter

    Just foundCourse <-
        runSelectReturningFirst $
            select $
                courseByIdQuery courseId

    return (chapter, foundCourse)

selectAllCourseChapters :: Int32 -> AppM [ChapterReturnType]
selectAllCourseChapters =
    executeBeamDebug
        . runSelectReturningList
        . select
        . allCourseChaptersQ

selectChapterByCourseIdAndNumber :: Int32 -> Int32 -> AppM (Maybe ChapterReturnType)
selectChapterByCourseIdAndNumber courseId chapterNumber =
    executeBeamDebug $
        runSelectReturningFirst $
            select $
                chapterByCourseIdAndNumberQ courseId chapterNumber

deleteChapter :: Int32 -> Int32 -> AppM ()
deleteChapter courseId chapterNumber =
    executeBeamDebug $
        runDelete $
            deleteChapterQ courseId chapterNumber

toChapterType :: ChapterReturnType -> Chapter.Chapter
toChapterType =
    Chapter.Chapter
        <$> _chapterChapterNumber . fst
        <*> _chapterTitle . fst
        <*> _chapterDescription . fst
        <*> _chapterBanner . fst
        <*> toCourseType . snd
