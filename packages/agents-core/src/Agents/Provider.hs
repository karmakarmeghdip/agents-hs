module Agents.Provider
    ( -- * Provider typeclass
      Provider(..)
      -- * Re-exports
    , module Agents.Types
    , module Agents.Error
    , module Agents.StreamEvent
    , module Agents.Tool
    , module Agents.Schema
    ) where

import Data.Conduit (ConduitT)
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error)

import Agents.Error (ProviderError)
import Agents.Types
    ( Message(..)
    , MessageRole(..)
    , ContentBlock(..)
    , CompletionResponse(..)
    , UsageInfo(..)
    , GenerationConfig(..)
    , ProviderConfig(..)
    , ToolCall(..)
    , ToolResult(..)
    )
import Agents.StreamEvent (StreamEvent(..))
import Agents.Tool (ToolDefinition(..))
import Agents.Schema (JsonSchema(..), jsonSchema)

-- | The core abstraction for LLM providers.
--
-- === Architecture Overview
--
-- @Provider@ is a typeclass that abstracts over different LLM backends
-- (OpenAI, Claude, etc.). Any type that implements @Provider@ can be
-- used interchangeably in agent code, making it easy to swap LLMs
-- without changing business logic.
--
-- === Effect System: Effectful
--
-- All Provider methods run in @Eff es@ with two required effects:
--
-- * @IOE :> es@ - for IO operations (HTTP requests, streaming)
-- * @Error ProviderError :> es@ - for error handling via Effectful's
--   Error effect. Errors are thrown with @throwError@ and caught
--   with @catchError@ or @runError@.
--
-- === Design Decision: Separate Operations
--
-- Following the decision to have separate operations rather than a
-- unified @complete@ method, the Provider typeclass exposes three
-- distinct methods:
--
-- 1. @generate@ - One-shot text generation (returns full response)
-- 2. @stream@ - Streaming generation (yields events via Conduit)
-- 3. @respond@ - Generation with tool support (handles the tool-use
--    response pattern where the LLM may request tool calls)
--
-- This separation allows:
--
-- * Simpler method signatures (no need for a \"mode\" parameter)
-- * Provider-specific optimizations per operation
-- * Clear intent at the call site
--
-- === Usage Example (Conceptual)
--
-- @
-- {\-# LANGUAGE OverloadedStrings #-\}
--
-- runEff $ do
--     let config = ProviderConfig
--             { pcApiKey  = "sk-..."
--             , pcBaseUrl = "https://api.openai.com"
--             }
--     let genConfig = GenerationConfig
--             { gcModel          = "gpt-4o"
--             , gcMaxTokens      = Just 1024
--             , gcTemperature    = Just 0.7
--             , gcTopP           = Nothing
--             , gcStopSequences  = Nothing
--             , gcResponseSchema = Nothing
--             }
--     let messages = [Message User [TextContent "Hello!"]]
--
--     -- Using OpenAI provider:
--     result <- generate openAIProvider config genConfig messages []
--     -- OR using Claude provider:
--     -- result <- generate claudeProvider config genConfig messages []
-- @
--
-- === Provider Adapter Implementation Guide
--
-- To implement a new provider:
--
-- 1. Create a data type (e.g., @OpenAIProvider@) - can be unit type if
--    no runtime configuration is needed beyond 'ProviderConfig'
-- 2. Implement all methods of the @Provider@ typeclass
-- 3. Convert between agnostic types ('Message', 'ContentBlock', etc.)
--    and the provider's native types
-- 4. Handle provider-specific errors by mapping to 'ProviderError'
--
-- See @Agents.Provider.OpenAI@ and @Agents.Provider.Claude@ for examples.
class Provider p where

    -- | Generate a completion (non-streaming).
    --
    -- Sends the conversation messages and optional tool definitions
    -- to the LLM and waits for the complete response.
    --
    -- === When to use
    --
    -- * Simple text generation
    -- * Structured output (with @gcResponseSchema@)
    -- * Tool calling where you don't need streaming
    --
    -- === Implementation Notes
    --
    -- * OpenAI: Uses the Chat Completions API (POST /v1/chat/completions)
    -- * Claude: Uses the Messages API (POST /v1/messages)
    -- * Both: Convert messages/tools to native format, send request,
    --   parse response, convert back to agnostic types
    generate
        :: (IOE :> es, Error ProviderError :> es)
        => p
        -> ProviderConfig
        -> GenerationConfig
        -> [Message]
        -> [ToolDefinition]
        -> Eff es CompletionResponse

    -- | Stream a completion, yielding events via Conduit.
    --
    -- Starts a streaming request and returns a Conduit source that
    -- yields 'StreamEvent' values as they arrive from the LLM.
    --
    -- === When to use
    --
    -- * Real-time text display (show tokens as they arrive)
    -- * Progressive UI updates
    -- * Long-running generations where you want to start processing
    --   before the full response is complete
    --
    -- === Conduit Integration
    --
    -- The returned @ConduitT () StreamEvent (Eff es) ()@ is a Conduit
    -- source. Connect it to a sink to process events:
    --
    -- @
    -- source <- stream provider config genConfig messages tools
    -- runConduit $ source .| CL.mapM_ handleEvent
    -- @
    --
    -- === Implementation Notes
    --
    -- * OpenAI: Uses the Chat Completions API with @stream: true@
    -- * Claude: Uses the Messages API with @stream: true@
    -- * Both: Parse SSE (Server-Sent Events), convert deltas to
    --   StreamEvent values, yield them through the Conduit
    --   Must handle: text deltas, tool call start/delta/end,
    --   usage info, and errors
    stream
        :: (IOE :> es, Error ProviderError :> es)
        => p
        -> ProviderConfig
        -> GenerationConfig
        -> [Message]
        -> [ToolDefinition]
        -> Eff es (ConduitT () StreamEvent (Eff es) ())

    -- | Generate a response with tool support.
    --
    -- Similar to @generate@, but specifically designed for the tool-use
    -- pattern where the LLM may respond with tool calls that need to be
    -- executed and fed back.
    --
    -- === When to use
    --
    -- * Agent loops where the LLM calls tools
    -- * Multi-step reasoning with tool assistance
    -- * Any scenario where the LLM response may contain @ToolCallContent@
    --
    -- === Design Decision: Separate from generate
    --
    -- While @generate@ can also return tool calls (they appear as
    -- @ToolCallContent@ in the response), @respond@ is a separate
    -- operation because:
    --
    -- * Some providers have a different API for tool-use responses
    --   (e.g., OpenAI's Responses API vs Chat Completions API)
    -- * The request format may differ (e.g., tool role messages
    --   are handled differently)
    -- * It makes intent clearer at the call site
    --
    -- === Implementation Notes
    --
    -- * OpenAI: Prefer the Responses API for tool calling,
    --   fall back to Chat Completions with tools
    -- * Claude: Use the Messages API with tools parameter
    -- * Both: Convert tool definitions and tool results to native format
    respond
        :: (IOE :> es, Error ProviderError :> es)
        => p
        -> ProviderConfig
        -> GenerationConfig
        -> [Message]
        -> [ToolDefinition]
        -> Eff es CompletionResponse