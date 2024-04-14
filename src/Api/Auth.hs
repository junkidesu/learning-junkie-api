{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Api.Auth (AuthAPI, authServer) where

import qualified Codec.Binary.UTF8.Generic as BS
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Password.Bcrypt (PasswordCheck (PasswordCheckFail, PasswordCheckSuccess), PasswordHash (PasswordHash), checkPassword, mkPassword)
import Data.Pool (Pool)
import qualified Data.Text as T
import Data.Time
import Database.Operations.Users (userByEmail)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Auth.Server
import qualified Types.Auth.Credentials as C
import qualified Types.Auth.Response as AR
import qualified Types.Auth.User as AU
import qualified Types.User as U

type Login =
  Summary "Login to the application"
    :> "login"
    :> ReqBody '[JSON] C.Credentials
    :> PostCreated '[JSON] AR.AuthResponse

type AuthAPI = "auth" :> Login

authServer :: Pool Connection -> JWTSettings -> Server AuthAPI
authServer conns jwts = login
 where
  login :: C.Credentials -> Handler AR.AuthResponse
  login credentials = do
    foundUser <- liftIO $ userByEmail conns (T.strip . C.email $ credentials)

    case foundUser of
      Nothing -> throwError err401
      Just user ->
        let passwordCheck =
              checkPassword
                (mkPassword . T.strip . C.password $ credentials)
                (PasswordHash . U.passwordHash $ user)
         in case passwordCheck of
              PasswordCheckFail -> throwError err401
              PasswordCheckSuccess -> do
                let
                  authUser = AU.AuthUser (U.id user) (U.email user) (U.role user)

                tokenExpireTime <- addUTCTime (3600 :: NominalDiffTime) <$> liftIO getCurrentTime

                eitherToken <-
                  liftIO $
                    makeJWT
                      authUser
                      jwts
                      (Just tokenExpireTime)

                case eitherToken of
                  Left _ -> throwError err401
                  Right tokenBS -> do
                    let token = T.pack . BS.toString $ tokenBS
                        authResponse = AR.AuthResponse (U.id user) token
                    return authResponse
