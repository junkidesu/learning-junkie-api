{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.User (User (..)) where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Data (Proxy (Proxy))
import Data.Swagger (
    HasProperties (properties),
    HasRequired (required),
    NamedSchema (NamedSchema),
    ToSchema (declareNamedSchema),
    declareSchemaRef,
 )
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Database.PostgreSQL.Simple (FromRow)
import GHC.Generics (Generic)
import Types.User.Education (Education)
import Types.User.Role
import Prelude hiding (id)

data User = User
    { id :: !Int
    , joined :: !UTCTime
    , name :: !(Maybe Text)
    , birthday :: !(Maybe Day)
    , education :: !(Maybe Education)
    , role :: Role
    , email :: !Text
    , passwordHash :: !Text
    }
    deriving (Eq, Read, Show, Generic)

instance FromJSON User
instance ToJSON User where
    toJSON :: User -> Value
    toJSON user =
        object
            [ "id" .= id user
            , "joined" .= joined user
            , "name" .= name user
            , "birthday" .= birthday user
            , "education" .= education user
            , "role" .= role user
            , "email" .= email user
            ]

instance FromRow User
instance ToSchema User where
    declareNamedSchema _ = do
        intSchema <- declareSchemaRef (Proxy :: Proxy Int)
        textSchema <- declareSchemaRef (Proxy :: Proxy Text)
        timeSchema <- declareSchemaRef (Proxy :: Proxy UTCTime)
        daySchema <- declareSchemaRef (Proxy :: Proxy Day)
        roleSchema <- declareSchemaRef (Proxy :: Proxy Role)
        educationSchema <- declareSchemaRef (Proxy :: Proxy Education)

        return $
            NamedSchema (Just "User") $
                mempty
                    & properties
                        .~ [ ("id", intSchema)
                           , ("joined", timeSchema)
                           , ("name", textSchema)
                           , ("birthday", daySchema)
                           , ("education", educationSchema)
                           , ("role", roleSchema)
                           , ("email", textSchema)
                           ]
                    & required .~ ["id", "joined", "role", "email"]
