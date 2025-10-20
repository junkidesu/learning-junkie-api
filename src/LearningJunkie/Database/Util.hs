{-# LANGUAGE FlexibleContexts #-}

module LearningJunkie.Database.Util where

import Control.Monad.Trans.Reader (asks)
import Data.Pool (withResource)
import Database.Beam
import Database.Beam.Backend (HasSqlValueSyntax)
import Database.Beam.Postgres (Connection, Pg, PgJSONB (PgJSONB), Postgres, runBeamPostgres, runBeamPostgresDebug)
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import LearningJunkie.Web.AppM (AppM)
import LearningJunkie.Web.Environment (Environment (dbConnection))

withConnection :: (Connection -> IO a) -> AppM a
withConnection op = do
    conns <- asks dbConnection

    liftIO $ withResource conns $ \conn ->
        op conn

executeBeam :: Pg a -> AppM a
executeBeam op = withConnection $ \conn -> runBeamPostgres conn op

executeBeamDebug :: Pg a -> AppM a
executeBeamDebug op = withConnection $ \conn -> runBeamPostgresDebug putStrLn conn op

updateIfChanged ::
    (HasSqlValueSyntax PgValueSyntax a) =>
    (tbl (QField s) -> C (QField s) a) ->
    tbl (QField s) ->
    Maybe a ->
    QAssignment Postgres s
updateIfChanged c r = maybe mempty (\attr -> c r <-. val_ attr)

fromJSONB :: PgJSONB a -> a
fromJSONB (PgJSONB a) = a

tripleFst :: (a, b, c) -> a
tripleFst (x, _, _) = x

tripleSnd :: (a, b, c) -> b
tripleSnd (_, x, _) = x

tripleThrd :: (a, b, c) -> c
tripleThrd (_, _, x) = x
