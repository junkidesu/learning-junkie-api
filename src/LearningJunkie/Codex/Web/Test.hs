{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module LearningJunkie.Codex.Web.Test where

import Control.Monad.IO.Class (MonadIO (liftIO))
import LearningJunkie.Codex (executeCode)
import LearningJunkie.Codex.ExecutionResult (ExecutionResult)
import LearningJunkie.Codex.TestInput (TestInput (environment, program))
import LearningJunkie.Web.AppM (AppM)
import Servant

type API =
        Summary "An endpoint for testing the output of a code"
                :> "test"
                :> ReqBody '[JSON] TestInput
                :> PostCreated '[JSON] ExecutionResult

handler :: TestInput -> AppM ExecutionResult
handler testInput = do
        liftIO $
                executeCode
                        (environment testInput)
                        (program testInput)
