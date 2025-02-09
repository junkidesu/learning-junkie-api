{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module LearningJunkie.Universities.Database where

import Data.Int (Int32)
import Data.Pool (Pool, withResource)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (runInsertReturningList))
import Database.Beam.Postgres
import LearningJunkie.Database (LearningJunkieDb (universities), connectToDb, db)
import LearningJunkie.Universities.Database.Table (University, UniversityT (University, _universityId))
import qualified LearningJunkie.Universities.University.Attributes as Attributes

type UniversityQuery s = Q Postgres LearningJunkieDb s (UniversityT (QExpr Postgres s))

allUniversitiesQuery :: UniversityQuery s
allUniversitiesQuery = all_ $ universities db

universityByIdQuery :: Int32 -> UniversityQuery s
universityByIdQuery id =
    filter_
        (\r -> _universityId r ==. val_ id)
        allUniversitiesQuery

insertUniversityQuery :: Attributes.New -> SqlInsert Postgres UniversityT
insertUniversityQuery newUniversity =
    insert (universities db) $
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
deleteUniversityQuery id = delete (universities db) (\r -> _universityId r ==. val_ id)

selectAllUniversities :: Pool Connection -> IO [University]
selectAllUniversities conns = withResource conns $ \conn ->
    runBeamPostgresDebug putStrLn conn $
        runSelectReturningList $
            select allUniversitiesQuery

selectUniversityById :: Pool Connection -> Int32 -> IO (Maybe University)
selectUniversityById conns id = withResource conns $ \conn -> do
    runBeamPostgresDebug putStrLn conn $
        runSelectReturningFirst $
            select $
                universityByIdQuery id

insertUniversity :: Pool Connection -> Attributes.New -> IO University
insertUniversity conns newUniversity = withResource conns $ \conn ->
    runBeamPostgresDebug putStrLn conn $ do
        [university] <-
            runInsertReturningList $
                insertUniversityQuery newUniversity
        return university

deleteUniversity :: Pool Connection -> Int32 -> IO ()
deleteUniversity conns id = withResource conns $ \conn ->
    runBeamPostgresDebug putStrLn conn $
        runDelete $
            (deleteUniversityQuery id)
