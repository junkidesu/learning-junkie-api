{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Users.Web.Login where

import Conduit (MonadIO (liftIO))
import Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString.Lazy.UTF8 as BS
import Data.Password.Bcrypt
import qualified Data.Text as T
import Data.Time
import qualified LearningJunkie.Universities.Database.Table as University
import LearningJunkie.Users.Database (selectUserByEmail)
import qualified LearningJunkie.Users.Database.Table as User
import LearningJunkie.Web.AppM (AppM)
import qualified LearningJunkie.Web.Auth.Credentials as Credentials
import LearningJunkie.Web.Auth.Response (AuthResponse (AuthResponse))
import LearningJunkie.Web.Auth.User (AuthUser (AuthUser))
import LearningJunkie.Web.Environment (Environment (jwtSettings))
import Servant
import Servant.Auth.Server (makeJWT)

type API =
    "login"
        :> Summary "Log in to the application"
        :> ReqBody '[JSON] Credentials.Credentials
        :> Post '[JSON] AuthResponse

handler :: Credentials.Credentials -> AppM AuthResponse
handler credentials = do
    jwts <- asks jwtSettings

    foundUser <- selectUserByEmail (T.strip . Credentials.email $ credentials)

    case foundUser of
        Nothing -> throwError err401
        Just (user, mbUniversity) ->
            let passwordCheck =
                    checkPassword
                        (mkPassword . T.strip . Credentials.password $ credentials)
                        (PasswordHash . User._userPasswordHash $ user)
             in case passwordCheck of
                    PasswordCheckFail -> throwError err401
                    PasswordCheckSuccess -> do
                        let
                            authUser =
                                AuthUser
                                    (User._userId user)
                                    (User._userEmail user)
                                    (User._userRole user)
                                    (University._universityId <$> mbUniversity)

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
                                    authResponse = AuthResponse (User._userId user) token
                                return authResponse
