{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Agents.Provider.OpenAI
  ( OpenAIProvider (..),
    newOpenAIProvider,
  )
where

import Agents.Error (ProviderError (..))
import Agents.Provider (Provider (..))
import Agents.Provider.OpenAI.Convert
import Agents.Provider.OpenAI.Streaming
import Agents.Provider.OpenAI.Types
import Agents.StreamEvent (StreamEvent (..))
import Agents.Tool (ToolDefinition (..))
import Agents.Types
  ( CompletionResponse (..),
    GenerationConfig (..),
    Message (..),
    ProviderConfig (..),
  )
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, newTChanIO, readTChan, writeTChan)
import Control.Exception (SomeException, try)
import Data.Conduit (ConduitT, yield)
import Data.Text qualified as Text
import Effectful (Eff, IOE, (:>))
import Effectful qualified as E
import Effectful.Error.Static (Error, throwError)
import OpenAI.V1 qualified as OpenAI
import OpenAI.V1.Chat.Completions qualified as CC

instance Provider OpenAIProvider where
  generate ::
    (IOE :> es, Error ProviderError :> es) =>
    OpenAIProvider ->
    ProviderConfig ->
    GenerationConfig ->
    [Message] ->
    [ToolDefinition] ->
    Eff es CompletionResponse
  generate provider _config genConfig messages tools = do
    oaiMessages <- either throwError pure (toOpenAIMessages messages)
    let mds = makeOAIMethods provider
        request = buildRequest oaiMessages genConfig tools
    result <-
      E.liftIO $
        try @(SomeException) $
          OpenAI.createChatCompletion mds request
    case result of
      Right resp -> pure $ fromOpenAIResponse resp
      Left exc -> throwError $ ConnectionError (Text.pack (show exc))

  stream ::
    (IOE :> es, Error ProviderError :> es) =>
    OpenAIProvider ->
    ProviderConfig ->
    GenerationConfig ->
    [Message] ->
    [ToolDefinition] ->
    Eff es (ConduitT () StreamEvent (Eff es) ())
  stream provider _config genConfig messages tools = do
    oaiMessages <- either throwError pure (toOpenAIMessages messages)
    let mds = makeOAIMethods provider
        request =
          (buildRequest oaiMessages genConfig tools)
            { CC.stream = Just True,
              CC.stream_options =
                Just
                  CC._ChatCompletionStreamOptions
                    { CC.include_usage = Just True,
                      CC.include_obfuscation = Nothing
                    }
            }
    chan <- E.liftIO newTChanIO
    _ <- E.liftIO $ forkIO $ do
      OpenAI.createChatCompletionStreamTyped mds request $ \event -> do
        atomically $ writeTChan chan (Just event)
      atomically $ writeTChan chan Nothing
    pure $ do
      let loop = do
            me <- E.liftIO $ atomically $ readTChan chan
            case me of
              Nothing -> pure ()
              Just (Left errMsg) -> yield (StreamError errMsg) >> loop
              Just (Right chunk) -> do
                mapM_ yield (chunkToStreamEvents chunk)
                loop
      loop
      yield StreamDone

  respond ::
    (IOE :> es, Error ProviderError :> es) =>
    OpenAIProvider ->
    ProviderConfig ->
    GenerationConfig ->
    [Message] ->
    [ToolDefinition] ->
    Eff es CompletionResponse
  respond = generate
