{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module LearningJunkie.Universities.Database where

import Control.Monad.Trans.Reader (asks)
import Data.Int (Int32)
import Data.Pool (Pool, withResource)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (runInsertReturningList))
import Database.Beam.Postgres
import LearningJunkie.Database
import LearningJunkie.Universities.Database.Table
import qualified LearningJunkie.Universities.University as University
import qualified LearningJunkie.Universities.University.Attributes as Attributes
import LearningJunkie.Web.AppM (AppM)
import Prelude hiding (id)

type UniversityQuery s = Q Postgres LearningJunkieDb s (UniversityT (QExpr Postgres s))

allUniversitiesQuery :: UniversityQuery s
allUniversitiesQuery = all_ $ dbUniversities db

universityByIdQuery :: Int32 -> UniversityQuery s
universityByIdQuery id =
    filter_
        (\r -> _universityId r ==. val_ id)
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

selectAllUniversities :: AppM [University]
selectAllUniversities =
    withConnection $ \conn ->
        runBeamPostgresDebug putStrLn conn $
            runSelectReturningList $
                select allUniversitiesQuery

selectUniversityById :: Int32 -> AppM (Maybe University)
selectUniversityById id = withConnection $ \conn -> do
    runBeamPostgresDebug putStrLn conn $
        runSelectReturningFirst $
            select $
                universityByIdQuery id

insertUniversity :: Attributes.New -> AppM University
insertUniversity newUniversity = withConnection $ \conn ->
    runBeamPostgresDebug putStrLn conn $ do
        [university] <-
            runInsertReturningList $
                insertUniversityQuery newUniversity
        return university

deleteUniversity :: Int32 -> AppM ()
deleteUniversity id = withConnection $ \conn ->
    runBeamPostgresDebug putStrLn conn $
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
