module Agents.StreamEvent
    ( StreamEvent(..)
    ) where

import Data.Text (Text)
import Data.Aeson (Value)

-- | Rich stream events yielded during streaming generation.
--
-- === Architecture Decision: Rich Stream Events
--
-- Following the AI SDK pattern, streaming yields a sequence of
-- structured events rather than raw text deltas. This allows
-- consumers to react to different event types in real time:
--
-- * Display text as it arrives
-- * Detect when a tool call starts, accumulates, and completes
-- * Track token usage for cost monitoring
-- * Handle errors mid-stream
--
-- === Conduit Integration
--
-- These events are yielded via @Conduit.Source@ in the Provider's
-- @stream@ method. Consumers use standard Conduit operations
-- (@await@, @connect@, @sinkList@, etc.) to process events:
--
-- @
-- result <- runEff . runError $ do
--     events <- stream provider config genConfig messages tools
--     runConduit $ events .| CL.mapM_ handleEvent
-- @
--
-- === Tool Call Streaming
--
-- Tool calls arrive incrementally in streaming mode:
--
-- 1. @'StreamToolCallStart'@ - LLM begins a tool call (may have partial args)
-- 2. @'StreamToolCallDelta'@ - argument fragments arrive
-- 3. @'StreamToolCallEnd'@ - tool call is complete with full arguments
--
-- OpenAI streams tool calls in chunks where the function name arrives
-- first, then arguments arrive as partial JSON strings. Claude uses a
-- similar pattern. The Provider adapter is responsible for assembling
-- these fragments.
--
-- === Design Note on ToolCall Reuse
--
-- 'StreamToolCallStart' and 'StreamToolCallEnd' intentionally use
-- inline fields rather than the 'Agents.Types.ToolCall' type to avoid
-- a circular module dependency. When the stream completes, the
-- agent code should construct a 'Agents.Types.ToolCall' from the
-- data received in these events.
data StreamEvent
    = StreamTextDelta Text
    -- ^ A chunk of text content. Concatenate these to build
    --   the full text response. Each delta is a small fragment
    --   of the complete text.

    | StreamToolCallStart Text Text
    -- ^ The LLM has started a tool call.
    --   Fields: tool call ID, tool name.
    --   Arguments may be empty at this point and arrive via deltas.

    | StreamToolCallDelta Text Value
    -- ^ A delta update to an in-progress tool call's arguments.
    --   Fields: tool call ID, accumulated argument JSON so far.
    --   The Value may be partial JSON during streaming.
    --   Only the final delta (before StreamToolCallEnd) has complete args.

    | StreamToolCallEnd Text Text Value
    -- ^ A tool call is complete with all arguments assembled.
    --   Fields: tool call ID, tool name, complete arguments as JSON Value.
    --   The consumer can now construct a 'Agents.Types.ToolCall'.

    | StreamThinkingDelta Text
    -- ^ An extended thinking/reasoning delta.
    --   Only applicable for models that support thinking
    --   (e.g., Claude's extended thinking feature).
    --   Concatenate deltas to get the full thinking text.

    | StreamUsage Int Int Int
    -- ^ Token usage information, typically sent at the end of
    --   a streaming response.
    --   Fields: prompt tokens, completion tokens, total tokens.

    | StreamError Text
    -- ^ An error occurred during streaming.
    --   The stream may or may not continue after this.
    --   If the stream terminates due to an error, this will be
    --   the last event before 'StreamDone'.

    | StreamDone
    -- ^ The stream has completed successfully.
    --   No more events will follow this.
    deriving (Show, Eq)