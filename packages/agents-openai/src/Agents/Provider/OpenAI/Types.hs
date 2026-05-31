{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Agents.Provider.OpenAI.Types
    ( OpenAIProvider(..)
    , newOpenAIProvider
    , makeOAIMethods
    ) where

import qualified Data.Text as Text
import Data.Text (Text)

import Servant.Client (ClientEnv)

import qualified OpenAI.V1 as OpenAI

import Agents.Types (ProviderConfig(..))

data OpenAIProvider = OpenAIProvider
    { oaiClientEnv :: ClientEnv
    , oaiApiKey    :: Text
    , oaiOrg      :: Maybe Text
    , oaiProject  :: Maybe Text
    }

newOpenAIProvider :: ProviderConfig -> IO OpenAIProvider
newOpenAIProvider config = do
    let baseUrlStr = if Text.null (pcBaseUrl config)
                       then "https://api.openai.com"
                       else pcBaseUrl config
    clientEnv <- OpenAI.getClientEnv baseUrlStr
    pure OpenAIProvider
        { oaiClientEnv = clientEnv
        , oaiApiKey    = pcApiKey config
        , oaiOrg       = Nothing
        , oaiProject   = Nothing
        }

makeOAIMethods :: OpenAIProvider -> OpenAI.Methods
makeOAIMethods provider =
    OpenAI.makeMethods
        (oaiClientEnv provider)
        (oaiApiKey provider)
        (oaiOrg provider)
        (oaiProject provider)
