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
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import GHC.Generics (Generic)
import Types.University (University)
import Types.User.Education (Education)
import Types.User.Role
import Prelude hiding (id)

data User = User
    { id :: !Int
    , joined :: !UTCTime
    , name :: !Text
    , birthday :: !(Maybe Day)
    , education :: !(Maybe Education)
    , role :: Role
    , email :: !Text
    , avatar :: !(Maybe Text)
    , passwordHash :: !Text
    , university :: !(Maybe University)
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
            , "avatar" .= avatar user
            , "university" .= university user
            ]

instance FromRow User where
    fromRow =
        User
            <$> field -- id
            <*> field -- joined
            <*> field -- name
            <*> field -- birthday
            <*> field -- education
            <*> field -- role
            <*> field -- email
            <*> field -- avatar
            <*> field -- passwordHash
            <*> fromRow -- university

instance ToSchema User where
    declareNamedSchema _ = do
        intSchema <- declareSchemaRef (Proxy :: Proxy Int)
        textSchema <- declareSchemaRef (Proxy :: Proxy Text)
        timeSchema <- declareSchemaRef (Proxy :: Proxy UTCTime)
        daySchema <- declareSchemaRef (Proxy :: Proxy Day)
        roleSchema <- declareSchemaRef (Proxy :: Proxy Role)
        educationSchema <- declareSchemaRef (Proxy :: Proxy Education)
        universitySchema <- declareSchemaRef (Proxy :: Proxy University)

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
                           , ("avatar", textSchema)
                           , ("university", universitySchema)
                           ]
                    & required .~ ["id", "joined", "name", "role", "email"]
