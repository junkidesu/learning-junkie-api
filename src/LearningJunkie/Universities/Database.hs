{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module LearningJunkie.Universities.Database where

import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (runInsertReturningList), MonadBeamUpdateReturning (runUpdateReturningList))
import Database.Beam.Postgres
import LearningJunkie.Database
import LearningJunkie.Database.Util
import LearningJunkie.Universities.Database.Table
import qualified LearningJunkie.Universities.University as University
import qualified LearningJunkie.Universities.University.Attributes as Attributes
import LearningJunkie.Web.AppM (AppM)
import Prelude hiding (id)

type UniversityExpr s = UniversityT (QExpr Postgres s)
type UniversityNullableExpr s = UniversityT (Nullable (QExpr Postgres s))
type UniversityQ s = Q Postgres LearningJunkieDb s (UniversityExpr s)

allUniversitiesQuery :: UniversityQ s
allUniversitiesQuery = all_ $ dbUniversities db

universityByIdQuery :: Int32 -> UniversityQ s
universityByIdQuery id =
    filter_
        (\r -> _universityId r ==. val_ id)
        allUniversitiesQuery

universitiesByNameQuery :: Text -> UniversityQ s
universitiesByNameQuery universityName =
    filter_
        (\r -> _universityName r `like_` val_ (universityName <> "%"))
        allUniversitiesQuery

insertUniversityQuery :: Attributes.New -> SqlInsert Postgres UniversityT
insertUniversityQuery newUniversity =
    insert (dbUniversities db) $
        insertExpressions
            [ University
                default_
                (val_ (Attributes.name newUniversity))
                (val_ (Attributes.abbreviation newUniversity))
                (val_ (Attributes.year newUniversity))
                (val_ (Attributes.url newUniversity))
                (val_ (Attributes.logo newUniversity))
                default_
            ]

deleteUniversityQuery :: Int32 -> SqlDelete Postgres UniversityT
deleteUniversityQuery id = delete (dbUniversities db) (\r -> _universityId r ==. val_ id)

updateUniversityQuery :: Int32 -> Attributes.Edit -> SqlUpdate Postgres UniversityT
updateUniversityQuery id editUniversity =
    update
        (dbUniversities db)
        ( \r ->
            updateIfChanged _universityName r (Attributes.name editUniversity)
                <> updateIfChanged _universityAbbreviation r (Attributes.abbreviation editUniversity)
                <> updateIfChanged _universityYear r (Attributes.year editUniversity)
                <> updateIfChanged _universityLogo r (Attributes.logo editUniversity)
                <> updateIfChanged _universityUrl r (Attributes.url editUniversity)
        )
        (\r -> _universityId r ==. val_ id)

selectAllUniversities :: AppM [University]
selectAllUniversities =
    executeBeamDebug $
        runSelectReturningList $
            select allUniversitiesQuery

selectUniversityById :: Int32 -> AppM (Maybe University)
selectUniversityById id =
    executeBeamDebug $
        runSelectReturningFirst $
            select $
                universityByIdQuery id

selectUniversitiesByName :: Text -> AppM [University]
selectUniversitiesByName universityName =
    executeBeamDebug $
        runSelectReturningList $
            select $
                universitiesByNameQuery universityName

insertUniversity :: Attributes.New -> AppM University
insertUniversity newUniversity = executeBeamDebug $ do
    [university] <-
        runInsertReturningList $
            insertUniversityQuery newUniversity
    return university

updateUniversity :: Int32 -> Attributes.Edit -> AppM University
updateUniversity universityId editUniversity =
    executeBeamDebug $ do
        [university] <-
            runUpdateReturningList $
                updateUniversityQuery universityId editUniversity
        return university

deleteUniversity :: Int32 -> AppM ()
deleteUniversity id =
    executeBeamDebug $
        runDelete $
            deleteUniversityQuery id

toUniversityType :: University -> University.University
toUniversityType =
    University.University
        <$> _universityId
        <*> _universityName
        <*> _universityAbbreviation
        <*> _universityYear
        <*> _universityUrl
        <*> _universityLogo
        <*> _universityJoined
