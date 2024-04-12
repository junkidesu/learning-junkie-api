{-# LANGUAGE OverloadedStrings #-}

module Cors (myCors) where

import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (corsMethods, corsRequestHeaders), cors, simpleCorsResourcePolicy)

myCors :: Middleware
myCors = cors (const $ Just policy)
  where
    policy =
        simpleCorsResourcePolicy
            { corsRequestHeaders = ["Content-Type", "Authorization"]
            , corsMethods = ["OPTIONS", "GET", "PUT", "POST", "DELETE"]
            }
