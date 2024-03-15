{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Universities (UniversitiesAPI, universitiesServer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database.Operations.Universities (allUniversities, insertUniversity)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Auth
import Servant.Auth.Server
import qualified Types.Auth.User as AU
import Types.University
import qualified Types.University.NewUniversity as NU
import Types.User.Role (Role (Admin))

type JWTAuth = Auth '[JWT] AU.AuthUser

type GetAllUniversities =
  Summary "Get all universities"
    :> Get '[JSON] [University]

type RegisterUniversity = JWTAuth :> Summary "Register a university" :> ReqBody '[JSON] NU.NewUniversity :> PostCreated '[JSON] University

type UniversitiesAPI = "universities" :> (GetAllUniversities :<|> RegisterUniversity)

universitiesServer :: Pool Connection -> Server UniversitiesAPI
universitiesServer conns = getAllUniversities :<|> registerUniversity
 where
  getAllUniversities :: Handler [University]
  getAllUniversities = liftIO $ allUniversities conns

  registerUniversity :: AuthResult AU.AuthUser -> NU.NewUniversity -> Handler University
  registerUniversity (Authenticated authUser) newUniversity =
    case AU.role authUser of
      Admin -> liftIO $ insertUniversity conns newUniversity
      _ -> throwError err401
  registerUniversity _ _ = throwError err401
