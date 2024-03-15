{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Universities (UniversitiesAPI, universitiesServer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database.Operations.Universities (allUniversities, insertUniversity)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Types.University
import qualified Types.University.NewUniversity as NU

type GetAllUniversities =
  Summary "Get all universities"
    :> Get '[JSON] [University]

type RegisterUniversity = Summary "Register a university" :> ReqBody '[JSON] NU.NewUniversity :> PostCreated '[JSON] University

type UniversitiesAPI = "universities" :> (GetAllUniversities :<|> RegisterUniversity)

universitiesServer :: Pool Connection -> Server UniversitiesAPI
universitiesServer conns = getAllUniversities :<|> registerUniversity
 where
  getAllUniversities :: Handler [University]
  getAllUniversities = liftIO $ allUniversities conns

  registerUniversity :: NU.NewUniversity -> Handler University
  registerUniversity newUniversity = liftIO $ insertUniversity conns newUniversity
