{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Agents.Provider.Claude.Types
    ( ClaudeProvider(..)
    , newClaudeProvider
    , makeClaudeMethods
    ) where

import qualified Data.Text as Text
import Data.Text (Text)

import Servant.Client (ClientEnv)

import qualified Claude.V1 as CV1

import Agents.Types (ProviderConfig(..))

data ClaudeProvider = ClaudeProvider
    { claudeClientEnv :: ClientEnv
    , claudeApiKey    :: Text
    , claudeVersion   :: Maybe Text
    }

newClaudeProvider :: ProviderConfig -> IO ClaudeProvider
newClaudeProvider config = do
    let baseUrlStr = if Text.null (pcBaseUrl config)
                       then "https://api.anthropic.com"
                       else pcBaseUrl config
    clientEnv <- CV1.getClientEnv baseUrlStr
    pure ClaudeProvider
        { claudeClientEnv = clientEnv
        , claudeApiKey    = pcApiKey config
        , claudeVersion   = Just "2023-06-01"
        }

makeClaudeMethods :: ClaudeProvider -> CV1.Methods
makeClaudeMethods provider =
    CV1.makeMethods
        (claudeClientEnv provider)
        (claudeApiKey provider)
        (claudeVersion provider)
