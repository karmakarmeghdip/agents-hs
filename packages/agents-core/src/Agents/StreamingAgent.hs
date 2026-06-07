{-# LANGUAGE OverloadedStrings #-}

module Agents.StreamingAgent
    ( -- * Streaming agent event types
      StreamingAgentEvent(..)
    , -- * Running streaming agents
      streamAgent
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, newTChanIO, readTChan, writeTChan, TChan)
import Data.Aeson (Value(..), object)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Conduit (ConduitT, runConduit, yield, (.|))
import qualified Data.Conduit.List as CL
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Effectful (Eff, IOE, (:>))
import qualified Effectful as E
import Effectful.Error.Static (Error, runError)

import Agents.Agent
    ( Agent(..)
    , AgentError(..)
    , AgentResult(..)
    , AgentStep(..)
    , executeToolCall
    , prependSystemPrompt
    )
import Agents.Error (ProviderError(..))
import Agents.StreamEvent (StreamEvent(..))
import Agents.Provider (Provider(..))
import Agents.Tool (toolRegistryDefs)
import Agents.Memory (MemoryProvider(..))
import Agents.Types
    ( Message(..)
    , MessageRole(..)
    , ContentBlock(..)
    , CompletionResponse(..)
    , ToolCall(..)
    , ToolResult(..)
    , UsageInfo(..)
    )

data StreamingAgentEvent
    = StreamingAgentTextDelta Text
    | StreamingAgentThinkingDelta Text
    | StreamingAgentToolCallStarted Text Text
    | StreamingAgentToolCallCompleted ToolCall
    | StreamingAgentToolResult ToolCall ToolResult
    | StreamingAgentStepComplete AgentStep
    | StreamingAgentComplete AgentResult
    | StreamingAgentError AgentError
    deriving (Show, Eq)

data StreamAccum = StreamAccum
    { saTextContent   :: !Text
    , saThinkingText   :: !Text
    , saToolCalls     :: ![ToolCall]
    , saFinishReason  :: !(Maybe Text)
    , saModel         :: !(Maybe Text)
    , saPromptTokens  :: !Int
    , saOutputTokens  :: !Int
    , saHadError      :: !Bool
    }

emptyStreamAccum :: StreamAccum
emptyStreamAccum = StreamAccum
    { saTextContent  = ""
    , saThinkingText  = ""
    , saToolCalls    = []
    , saFinishReason  = Nothing
    , saModel        = Nothing
    , saPromptTokens = 0
    , saOutputTokens = 0
    , saHadError     = False
    }

isEmptyResponse :: CompletionResponse -> Bool
isEmptyResponse resp =
    null [t | TextContent t <- crContent resp, not (T.null t)]
    && null [tc | ToolCallContent tc <- crContent resp]

hasBrokenToolCalls :: CompletionResponse -> Bool
hasBrokenToolCalls resp = any isBroken [tc | ToolCallContent tc <- crContent resp]
  where
    isBroken tc = case tcArguments tc of
        Object km -> null (KeyMap.toList km)
        _ -> False

streamAgent
    :: (Provider p, IOE :> es, Error AgentError :> es)
    => Agent p
    -> Message
    -> Eff es (ConduitT () StreamingAgentEvent (Eff es) ())
streamAgent agent userMsg = do
    E.liftIO $ mpAdd (agentMemory agent) userMsg
    chan <- E.liftIO newTChanIO
    stepCountRef <- E.liftIO $ newIORef (0 :: Int)
    _ <- E.liftIO $ forkIO $ runStreamingLoopIO agent chan stepCountRef []
    pure $ sourceTChan chan

sourceTChan :: (IOE :> es) => TChan StreamingAgentEvent -> ConduitT () StreamingAgentEvent (Eff es) ()
sourceTChan chan = do
    event <- E.liftIO $ atomically $ readTChan chan
    yield event
    case event of
        StreamingAgentComplete _ -> pure ()
        StreamingAgentError _    -> pure ()
        _ -> sourceTChan chan

runStreamingLoopIO
    :: Provider p
    => Agent p
    -> TChan StreamingAgentEvent
    -> IORef Int
    -> [AgentStep]
    -> IO ()
runStreamingLoopIO agent chan stepCountRef steps = do
    stepCount <- readIORef stepCountRef
    if stepCount >= agentMaxSteps agent
        then do
            let result = AgentResult
                    { arText         = ""
                    , arFinishReason = "max_steps_reached"
                    , arSteps        = steps
                    }
            atomically $ writeTChan chan (StreamingAgentComplete result)
        else do
            messages <- mpGet (agentMemory agent)
            let toolDefs = toolRegistryDefs (agentTools agent)
                messagesWithSystem = prependSystemPrompt (agentSystemPrompt agent) messages

            -- First try generate to verify it works in this context
            eStreamResult <- E.runEff $ runError @ProviderError $ do
                source <- stream (agentProvider agent)
                                (agentProviderCfg agent)
                                (agentGenerationCfg agent)
                                messagesWithSystem
                                toolDefs
                accumRef <- E.liftIO $ newIORef emptyStreamAccum
                runConduit $ source .| CL.mapM_ (\ev -> E.liftIO $ processStreamEventIO accumRef chan ev)
                finalAccum <- E.liftIO $ readIORef accumRef
                pure $ buildResponse finalAccum

            response <- case eStreamResult of
                Right resp | not (isEmptyResponse resp) && not (hasBrokenToolCalls resp) -> pure resp
                _ -> do
                    eGenResult <- E.runEff $ runError @ProviderError $
                        generate (agentProvider agent)
                                 (agentProviderCfg agent)
                                 (agentGenerationCfg agent)
                                 messagesWithSystem
                                 toolDefs
                    case eGenResult of
                        Right resp -> pure resp
                        Left (_, perr) -> do
                            atomically $ writeTChan chan (StreamingAgentError (AEProvider perr))
                            pure CompletionResponse
                                { crContent = []
                                , crModel = ""
                                , crFinishReason = "error"
                                , crUsage = UsageInfo 0 0 0
                                }

            let llmStep = StepLLM response
            mpAdd (agentMemory agent) (Message Assistant (crContent response))
            let toolCalls = [tc | ToolCallContent tc <- crContent response]
            if null toolCalls
                then do
                    atomically $ writeTChan chan (StreamingAgentStepComplete llmStep)
                    let result = AgentResult
                            { arText         = extractStreamingText response
                            , arFinishReason = crFinishReason response
                            , arSteps        = steps ++ [llmStep]
                            }
                    atomically $ writeTChan chan (StreamingAgentComplete result)
                else do
                    atomically $ writeTChan chan (StreamingAgentStepComplete llmStep)
                    eToolResults <- E.runEff $ runError @ProviderError $ mapM (executeToolCall (agentTools agent)) toolCalls
                    case eToolResults of
                        Left (_, perr) -> do
                            atomically $ writeTChan chan (StreamingAgentError (AEProvider perr))
                        Right toolResults -> do
                            let toolSteps = zipWith StepTool toolCalls toolResults
                            mapM_ (\(tc, tr) -> atomically $ writeTChan chan (StreamingAgentToolResult tc tr)) (zip toolCalls toolResults)
                            mapM_ (\s -> atomically $ writeTChan chan (StreamingAgentStepComplete s)) toolSteps
                            mapM_ (\tr -> mpAdd (agentMemory agent) (Message Tool [ToolResultContent tr])) toolResults
                            modifyIORef' stepCountRef (+1)
                            runStreamingLoopIO agent chan stepCountRef (steps ++ [llmStep] ++ toolSteps)

hadStreamError :: CompletionResponse -> Bool
hadStreamError resp = any isErrorBlock (crContent resp)
  where
    isErrorBlock (TextContent t) | "stream_error" `T.isInfixOf` t = True
    isErrorBlock _ = False

processStreamEventIO :: IORef StreamAccum -> TChan StreamingAgentEvent -> StreamEvent -> IO ()
processStreamEventIO accumRef chan = \case
    StreamTextDelta text -> do
        atomically $ writeTChan chan (StreamingAgentTextDelta text)
        modifyIORef' accumRef $ \acc -> acc { saTextContent = saTextContent acc <> text }
    StreamThinkingDelta text -> do
        atomically $ writeTChan chan (StreamingAgentThinkingDelta text)
        modifyIORef' accumRef $ \acc -> acc { saThinkingText = saThinkingText acc <> text }
    StreamToolCallStart callId callName -> do
        atomically $ writeTChan chan (StreamingAgentToolCallStarted callId callName)
        modifyIORef' accumRef $ \acc -> acc { saToolCalls = saToolCalls acc ++ [ToolCall callId callName (object [])] }
    StreamToolCallDelta callId argsValue -> do
        modifyIORef' accumRef $ \acc ->
            let newCalls = map (\tc -> if tcId tc == callId then tc { tcArguments = argsValue } else tc) (saToolCalls acc)
            in acc { saToolCalls = newCalls }
    StreamToolCallEnd callId callName args -> do
        modifyIORef' accumRef $ \acc ->
            let newCalls = map (\tc -> if tcId tc == callId then tc { tcName = callName, tcArguments = args } else tc) (saToolCalls acc)
            in acc { saToolCalls = newCalls }
        let completedTc = ToolCall callId callName args
        atomically $ writeTChan chan (StreamingAgentToolCallCompleted completedTc)
    StreamUsage promptTokens outputTokens _total -> do
        modifyIORef' accumRef $ \acc -> acc
            { saPromptTokens = promptTokens
            , saOutputTokens = outputTokens
            }
    StreamError _text -> do
        modifyIORef' accumRef $ \acc -> acc { saHadError = True }
    StreamDone -> pure ()

buildResponse :: StreamAccum -> CompletionResponse
buildResponse accum = CompletionResponse
    { crContent      = contentBlocks
    , crModel        = fromMaybe "" (saModel accum)
    , crFinishReason = fromMaybe "stop" (saFinishReason accum)
    , crUsage        = UsageInfo
        { uiPromptTokens     = saPromptTokens accum
        , uiCompletionTokens = saOutputTokens accum
        , uiTotalTokens      = saPromptTokens accum + saOutputTokens accum
        }
    }
  where
    contentBlocks =
        let textBlock = [TextContent (saTextContent accum) | not (T.null (saTextContent accum))]
            toolBlocks = map ToolCallContent (saToolCalls accum)
        in textBlock ++ toolBlocks

extractStreamingText :: CompletionResponse -> Text
extractStreamingText resp = T.concat [t | TextContent t <- crContent resp]