module LearningJunkie.Chapters.Database where

import Data.Int (Int32)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (runInsertReturningList))
import Database.Beam.Postgres (Postgres)
import qualified LearningJunkie.Chapters.Chapter as Chapter
import qualified LearningJunkie.Chapters.Chapter.Attributes as Attributes
import LearningJunkie.Chapters.Database.Table
import LearningJunkie.Courses.Database.Table (PrimaryKey (CourseId))
import LearningJunkie.Database (LearningJunkieDb (dbChapters), db)
import LearningJunkie.Database.Util (executeBeamDebug)
import LearningJunkie.Web.AppM (AppM)

type ChapterDBType s = ChapterT (QExpr Postgres s)
type ChapterQuery s =
    Q
        Postgres
        LearningJunkieDb
        s
        (ChapterDBType s)

allCourseChaptersQuery :: Int32 -> ChapterQuery s
allCourseChaptersQuery courseId =
    do
        chapter <- all_ $ dbChapters db

        let CourseId chapterCourseId = _chapterCourse chapter

        guard_ (chapterCourseId ==. val_ courseId)

        return chapter

insertChapterQuery :: Int32 -> Attributes.New -> SqlInsert Postgres ChapterT
insertChapterQuery courseId newChapter =
    insert (dbChapters db) $
        insertExpressions
            [ Chapter
                (val_ $ Attributes.chapterNumber newChapter)
                (CourseId $ val_ courseId)
                (val_ $ Attributes.title newChapter)
                (val_ $ Attributes.description newChapter)
                (val_ $ Attributes.banner newChapter)
            ]

insertChapter :: Int32 -> Attributes.New -> AppM Chapter
insertChapter courseId newChapter = executeBeamDebug $ do
    [chapter] <- runInsertReturningList $ insertChapterQuery courseId newChapter

    return chapter

selectAllCourseChapters :: Int32 -> AppM [Chapter]
selectAllCourseChapters =
    executeBeamDebug
        . runSelectReturningList
        . select
        . allCourseChaptersQuery

toChapterType :: Chapter -> Chapter.Chapter
toChapterType =
    Chapter.Chapter
        <$> _chapterChapterNumber
        <*> _chapterTitle
        <*> _chapterDescription
        <*> _chapterBanner
