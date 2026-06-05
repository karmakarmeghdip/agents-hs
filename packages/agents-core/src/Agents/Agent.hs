{-# LANGUAGE OverloadedStrings #-}

module Agents.Agent
    ( -- * Agent type
      Agent(..)
    , -- * Running agents
      runAgent
    , -- * Step types
      AgentStep(..)
    , -- * Result types
      AgentResult(..)
    , -- * Error types
      AgentError(..)
    ) where

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (fromJSON, Result(..), toJSON)
import Control.Exception (try, SomeException)

import Effectful (Eff, IOE, (:>), liftIO)
import Effectful.Error.Static (Error, throwError, runError)

import Agents.Types
    ( Message(..)
    , MessageRole(..)
    , ContentBlock(..)
    , CompletionResponse(..)
    , GenerationConfig
    , ProviderConfig
    , ToolCall(..)
    , ToolResult(..)
    )
import Agents.Provider (Provider(..))
import Agents.Error (ProviderError)
import Agents.Tool (ToolHandler(..), ToolRegistry, toolRegistryDefs)
import Agents.Memory (MemoryProvider(..))

-- | An autonomous agent that can use LLM providers and tools.
--
-- === Architecture
--
-- An @Agent@ bundles all the configuration needed to run an agentic loop:
--
-- * A 'Provider' instance (OpenAI, Claude, etc.) for LLM calls
-- * Provider configuration (API key, base URL)
-- * Generation configuration (model, temperature, etc.)
-- * A 'ToolRegistry' mapping tool names to typed handlers
-- * A 'MemoryProvider' for conversation history persistence
-- * A maximum number of steps to prevent infinite loops
-- * An optional system prompt prepended to every conversation
--
-- === Creating and Running an Agent
--
-- @
-- do
--     mem <- newInMemoryMemory
--     let agent = Agent
--             { agentProvider      = openaiProvider
--             , agentProviderCfg   = ProviderConfig "sk-..." "https://api.openai.com"
--             , agentGenerationCfg = GenerationConfig "gpt-4o" Nothing (Just 0.7) Nothing Nothing Nothing
--             , agentTools         = Map.fromList [("get_weather", weatherHandler)]
--             , agentMemory        = mem
--             , agentMaxSteps      = 10
--             , agentSystemPrompt  = Just "You are a helpful assistant."
--             }
--     result <- runEff . runError $ runAgent agent (Message User [TextContent "Hello!"])
-- @
data Agent p = Agent
    { agentProvider      :: p
    -- ^ The LLM provider instance (e.g., OpenAIProvider, ClaudeProvider).

    , agentProviderCfg   :: ProviderConfig
    -- ^ Provider configuration (API key, base URL).

    , agentGenerationCfg :: GenerationConfig
    -- ^ Generation configuration (model, temperature, max tokens, etc.).

    , agentTools         :: ToolRegistry
    -- ^ Registry mapping tool names to their typed handlers.
    --   The agent loop uses this to look up and execute tool calls from the LLM.

    , agentMemory        :: MemoryProvider
    -- ^ Memory provider for conversation history persistence.
    --   In-memory for simple use cases; swap for database-backed providers
    --   for persistence across sessions.

    , agentMaxSteps      :: Int
    -- ^ Maximum number of LLM calls before the agent loop terminates.
    --   Prevents infinite loops from tool-calling cycles.
    --   Must be >= 1.

    , agentSystemPrompt  :: Maybe Text
    -- ^ Optional system prompt prepended to every conversation.
    --   Not stored in memory; always added as the first message
    --   when building the LLM request.
    }

-- | A single step in the agent's execution trace.
--
-- The agent loop produces a list of 'AgentStep's that record
-- every action the agent took. This allows users to inspect
-- the full trajectory: which LLM calls were made, which tools
-- were invoked, and what results were returned.
data AgentStep
    = StepLLM CompletionResponse
    -- ^ The LLM generated a response. Contains the full
    --   'CompletionResponse' including content blocks,
    --   finish reason, and usage info.

    | StepTool ToolCall ToolResult
    -- ^ A tool was invoked and produced a result.
    --   Use 'trIsError' on the 'ToolResult' to determine
    --   if the tool execution succeeded or failed.
    deriving (Show, Eq)

-- | The result of running an agent to completion.
data AgentResult = AgentResult
    { arText         :: Text
    -- ^ The final text output from the agent.
    --   Concatenation of all 'TextContent' blocks in the
    --   last LLM response. May be empty if the agent
    --   stopped mid-loop (e.g., max steps reached during
    --   a tool-calling cycle).

    , arFinishReason :: Text
    -- ^ Why the agent stopped.
    --   Common values: @"stop"@ (normal completion),
    --   @"max_steps_reached"@ (hit iteration limit),
    --   provider-specific values like @"length"@, @"content_filter"@.

    , arSteps :: [AgentStep]
    -- ^ Complete trace of all steps the agent took,
    --   in chronological order.
    } deriving (Show, Eq)

-- | Errors that can occur during agent execution.
--
-- Agent-specific errors that extend 'ProviderError' with additional
-- failure modes unique to the agentic loop.
data AgentError
    = AEProvider ProviderError
    -- ^ A provider API error occurred during an LLM call.
    --   Wraps the original 'ProviderError' from the provider.

    | AEMaxStepsReached Int
    -- ^ The agent exceeded its maximum number of LLM call steps.
    --   The Int is the configured max steps value.
    deriving (Show, Eq)

-- | Run an agent with a user message.
--
-- This is the main entry point for executing an agent. It:
--
-- 1. Adds the user message to the agent's memory
-- 2. Runs the agentic loop: call LLM, check for tool calls,
--    execute tools, feed results back, repeat
-- 3. Stops when the LLM responds without tool calls or
--    when the maximum number of steps is reached
--
-- The system prompt (if set) is prepended to the message list
-- on every LLM call, but not stored in memory.
--
-- === Error Handling
--
-- * Provider errors (API failures) are caught and re-thrown as 'AEProvider'
-- * Tool execution errors are caught and fed back to the LLM as error
--   tool results, allowing the LLM to correct its approach
-- * If max steps is reached, the loop terminates with
--   @arFinishReason = "max_steps_reached"@
runAgent
    :: (Provider p, IOE :> es, Error AgentError :> es)
    => Agent p
    -> Message
    -> Eff es AgentResult
runAgent agent userMsg = do
    liftIO $ mpAdd (agentMemory agent) userMsg
    go 0 []
  where
    go stepCount steps
        | stepCount >= agentMaxSteps agent =
            pure AgentResult
                { arText         = ""
                , arFinishReason = "max_steps_reached"
                , arSteps        = steps
                }
        | otherwise = do
            messages <- liftIO $ mpGet (agentMemory agent)
            let toolDefs = toolRegistryDefs (agentTools agent)
                messagesWithSystem = prependSystemPrompt (agentSystemPrompt agent) messages

            eResponse <- runError $ generate (agentProvider agent)
                                              (agentProviderCfg agent)
                                              (agentGenerationCfg agent)
                                              messagesWithSystem
                                              toolDefs
            case eResponse of
                Left (_, err) -> throwError $ AEProvider err
                Right response -> do
                    let llmStep = StepLLM response
                    liftIO $ mpAdd (agentMemory agent) (Message Assistant (crContent response))

                    let toolCalls = [tc | ToolCallContent tc <- crContent response]

                    if null toolCalls
                        then pure AgentResult
                            { arText         = extractText (crContent response)
                            , arFinishReason = crFinishReason response
                            , arSteps        = steps ++ [llmStep]
                            }
                        else do
                            toolResults <- mapM (executeToolCall (agentTools agent)) toolCalls
                            let toolSteps = zipWith StepTool toolCalls toolResults
                            liftIO $ mapM_ (\tr -> mpAdd (agentMemory agent) (Message Tool [ToolResultContent tr])) toolResults
                            go (stepCount + 1) (steps ++ [llmStep] ++ toolSteps)

-- | Execute a single tool call using the tool registry.
--
-- Handles three failure modes:
--
-- 1. Tool not found in registry → error tool result
-- 2. Arguments fail 'FromJSON' parsing → error tool result
-- 3. Handler throws an exception → error tool result
--
-- All failures are fed back to the LLM as error tool results,
-- allowing it to correct its approach.
executeToolCall
    :: (IOE :> es)
    => ToolRegistry
    -> ToolCall
    -> Eff es ToolResult
executeToolCall registry tc =
    case Map.lookup (tcName tc) registry of
        Nothing ->
            let result = ToolResult
                    { trToolCallId = tcId tc
                    , trResult     = toJSON @Text ("Unknown tool: " <> tcName tc)
                    , trIsError    = True
                    }
            in pure result
        Just (ToolHandler _ handlerFn) ->
            case fromJSON (tcArguments tc) of
                Error err ->
                    let result = ToolResult
                            { trToolCallId = tcId tc
                            , trResult     = toJSON @Text ("Invalid arguments: " <> T.pack err)
                            , trIsError    = True
                            }
                    in pure result
                Success args -> do
                    eResult <- liftIO $ try @SomeException (handlerFn args)
                    case eResult of
                        Left _ex ->
                            let result = ToolResult
                                    { trToolCallId = tcId tc
                                    , trResult     = toJSON @Text "Tool execution error"
                                    , trIsError    = True
                                    }
                            in pure result
                        Right val ->
                            let result = ToolResult
                                    { trToolCallId = tcId tc
                                    , trResult     = val
                                    , trIsError    = False
                                    }
                            in pure result

prependSystemPrompt :: Maybe Text -> [Message] -> [Message]
prependSystemPrompt Nothing msgs = msgs
prependSystemPrompt (Just sys) msgs = Message System [TextContent sys] : msgs

extractText :: [ContentBlock] -> Text
extractText blocks = T.concat [t | TextContent t <- blocks]