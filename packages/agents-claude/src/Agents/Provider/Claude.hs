{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Agents.Provider.Claude
    ( ClaudeProvider(..)
    , newClaudeProvider
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, newTChanIO, readTChan, writeTChan)
import Control.Exception (SomeException, try)
import Data.Conduit (ConduitT, yield)
import Data.IORef (newIORef)
import qualified Data.Text as Text

import Effectful (Eff, IOE, (:>))
import qualified Effectful as E
import Effectful.Error.Static (Error, throwError)

import qualified Claude.V1 as CV1
import qualified Claude.V1.Messages as CM

import Agents.Error (ProviderError(..))
import Agents.Types
    ( Message(..)
    , CompletionResponse(..)
    , GenerationConfig(..)
    , ProviderConfig(..)
    )
import Agents.StreamEvent (StreamEvent(..))
import Agents.Tool (ToolDefinition(..))
import Agents.Provider (Provider(..))

import Agents.Provider.Claude.Types
import Agents.Provider.Claude.Convert
import Agents.Provider.Claude.Streaming

instance Provider ClaudeProvider where

    generate :: (IOE :> es, Error ProviderError :> es)
             => ClaudeProvider
             -> ProviderConfig
             -> GenerationConfig
             -> [Message]
             -> [ToolDefinition]
             -> Eff es CompletionResponse
    generate provider _config genConfig messages tools = do
        (systemPrompt, nonSystemMsgs) <- either throwError pure (partitionMessages messages)
        claudeMessages <- either throwError pure (toClaudeMessages nonSystemMsgs)
        let mds = makeClaudeMethods provider
            request = buildRequest claudeMessages systemPrompt genConfig tools
        result <- E.liftIO $ try @(SomeException) $
            CV1.createMessage mds request
        case result of
            Right resp -> pure $ fromClaudeResponse resp
            Left exc   -> throwError $ ConnectionError (Text.pack (show exc))

    stream :: (IOE :> es, Error ProviderError :> es)
           => ClaudeProvider
           -> ProviderConfig
           -> GenerationConfig
           -> [Message]
           -> [ToolDefinition]
           -> Eff es (ConduitT () StreamEvent (Eff es) ())
    stream provider _config genConfig messages tools = do
        (systemPrompt, nonSystemMsgs) <- either throwError pure (partitionMessages messages)
        claudeMessages <- either throwError pure (toClaudeMessages nonSystemMsgs)
        let mds = makeClaudeMethods provider
            request = (buildRequest claudeMessages systemPrompt genConfig tools)
                { CM.stream = Just True
                }
        inputTokensRef <- E.liftIO $ newIORef (0 :: Int)
        chan <- E.liftIO newTChanIO
        _ <- E.liftIO $ forkIO $ do
            CV1.createMessageStreamTyped mds request $ \event -> do
                atomically $ writeTChan chan (Just event)
            atomically $ writeTChan chan Nothing
        pure $ do
            let loop = do
                    me <- E.liftIO $ atomically $ readTChan chan
                    case me of
                        Nothing -> pure ()
                        Just (Left errMsg) -> yield (StreamError errMsg) >> loop
                        Just (Right streamEvent) -> do
                            events <- E.liftIO $ processStreamEvent inputTokensRef streamEvent
                            mapM_ yield events
                            loop
            loop
            yield StreamDone

    respond :: (IOE :> es, Error ProviderError :> es)
            => ClaudeProvider
            -> ProviderConfig
            -> GenerationConfig
            -> [Message]
            -> [ToolDefinition]
            -> Eff es CompletionResponse
    respond = generate
