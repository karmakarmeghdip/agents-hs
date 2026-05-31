module Agents.Provider.Claude
    ( ClaudeProvider(..)
    , newClaudeProvider
    ) where

import Data.Conduit (ConduitT)
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error)

import Agents.Error (ProviderError)
import Agents.Types
    ( Message
    , GenerationConfig
    , ProviderConfig
    , CompletionResponse
    )
import Agents.Tool (ToolDefinition)
import Agents.StreamEvent (StreamEvent)
import Agents.Provider (Provider(..))

-- | Anthropic Claude provider type.
--
-- === Architecture Decision: Typeclass Instance Carrier
--
-- @ClaudeProvider@ is the type that carries the @Provider ClaudeProvider@
-- instance. It wraps the configuration needed to create a Claude client.
--
-- The @newClaudeProvider@ smart constructor initializes the servant-client
-- environment needed by the @claude@ Hackage package. This includes:
--
-- * Creating an HTTP connection manager
-- * Setting up TLS settings
-- * Building the servant @ClientEnv@
--
-- === Client Environment
--
-- The @claude@ package uses servant-client for HTTP communication.
-- Its @Claude.V1.Methods@ record contains all API methods and is
-- created by @Claude.V1.makeMethods@, which requires a @ClientEnv@
-- and the Anthropic API version header.
--
-- === Implementation Plan
--
-- The @Provider ClaudeProvider@ instance will:
--
-- 1. Convert agnostic types to Claude's native types:
--    * @Message@ → @Claude.V1.Messages.Message@
--    * @ContentBlock@ → @Claude.V1.Messages.ContentBlock@
--    * @ToolDefinition@ → @Claude.V1.Tool.Tool@
--    * @GenerationConfig@ → Claude request parameters
--
-- 2. Call the Claude API via servant-client
--
-- 3. Convert Claude's response types back to agnostic types:
--    * @Claude.V1.Messages.MessageResponse@ → @CompletionResponse@
--    * Claude SSE events → @StreamEvent@
--    * Claude tool_use blocks → @ToolCall@
--
-- 4. Map Claude-specific errors to @ProviderError@
--
-- === Claude API Specifics
--
-- Claude's API has some differences from OpenAI:
--
-- * Messages API uses @content@ as a list of content blocks
-- * System messages are a top-level @system@ parameter, not a message role
-- * @max_tokens@ is required (not optional)
-- * Tool calling uses @tool_use@ content blocks in responses
-- * Streaming uses SSE with event types: message_start, content_block_start,
--   content_block_delta, content_block_stop, message_delta, message_stop
-- * The anthropic-version header is required (@2023-06-01@)
data ClaudeProvider = ClaudeProvider
    { claudeClientEnv :: ()
    -- TODO: Replace () with the actual servant-client ClientEnv type
    -- from the claude package. This will be initialized by
    -- newClaudeProvider and used for all API calls.
    --
    -- Type will be: ClientEnv (from servant-client)
    --
    -- Also need to store the anthropic-version header value.
    -- The claude package's makeMethods takes an optional
    -- version parameter: Maybe Text
    }

-- | Smart constructor for creating a Claude provider.
--
-- === Implementation Plan
--
-- 1. Parse the @ProviderConfig@ to get the API key and base URL
-- 2. If @pcBaseUrl@ is empty, default to @"https://api.anthropic.com"@
-- 3. Create an HTTP connection manager with TLS settings
-- 4. Build a @ClientEnv@ with the connection manager
-- 5. Return @ClaudeProvider@ with the client environment
--
-- === Example (conceptual)
--
-- @
-- provider <- newClaudeProvider (ProviderConfig "sk-ant-..." "")
-- result <- generate provider genConfig messages tools
-- @
--
-- === Claude API Version Header
--
-- Claude requires an @anthropic-version@ header. The default is
-- @2023-06-01@. The @claude@ package handles this via makeMethods'
-- version parameter.
newClaudeProvider :: ProviderConfig -> IO ClaudeProvider
newClaudeProvider _config = error "newClaudeProvider: not yet implemented"
-- TODO: Implement using Claude.V1.getClientEnv and makeMethods
-- from the claude Hackage package.
--
-- Steps:
-- 1. Determine base URL from ProviderConfig (default: https://api.anthropic.com)
-- 2. Call Claude.V1.getClientEnv with the base URL
-- 3. Return ClaudeProvider with the client env stored
--
-- Reference from claude package:
--   clientEnv <- getClientEnv "https://api.anthropic.com"
--   let Methods{..} = makeMethods clientEnv apiKey (Just "2023-06-01")

-- | Provider instance for Anthropic's Claude.
--
-- === Implementation Plan
--
-- Each method follows the same general pattern:
--
-- 1. **Convert inputs**: Transform agnostic types to Claude's native types
--    (Message → Claude Message, ToolDefinition → Claude Tool, etc.)
--
-- 2. **Call API**: Use the claude package's Methods record to call
--    the Messages API
--
-- 3. **Convert outputs**: Transform Claude's response back to agnostic types
--    (MessageResponse → CompletionResponse, etc.)
--
-- 4. **Handle errors**: Map Claude/servant errors to ProviderError
--
-- === Key Conversions
--
-- Message → Claude Message:
--   System role → Claude's top-level @system@ parameter
--   User role → Claude.V1.Messages.Message{ role = User }
--   Assistant role → Claude.V1.Messages.Message{ role = Assistant }
--   Tool role → Claude's tool_result content block
--
--   IMPORTANT: Claude handles system messages differently from OpenAI.
--   System messages are extracted from the message list and passed
--   as the top-level @system@ parameter, not as a message role.
--
-- ContentBlock → Claude content:
--   TextContent t → Claude.V1.Messages.textContent t
--   ImageContent → Claude.V1.Messages.imageContentBlock
--   ToolCallContent → Claude.V1.Messages.toolUseContentBlock
--   ToolResultContent → Claude.V1.Messages.toolResultContentBlock
--   EscapeHatchContent → pass through as raw JSON
--
-- ToolDefinition → Claude Tool:
--   ToolDefinition{..} → Claude.V1.Tool.Tool{ name, description, input_schema }
--
-- === Streaming Implementation
--
-- Claude's streaming SSE events need to be mapped to StreamEvent:
--
--   message_start → (metadata, model info - can be captured for UsageInfo)
--   content_block_start (type=text) → first text delta
--   content_block_delta (type=text_delta) → StreamTextDelta
--   content_block_start (type=tool_use) → StreamToolCallStart
--   content_block_delta (type=input_json_delta) → StreamToolCallDelta
--   content_block_stop → StreamToolCallEnd (for tool_use blocks)
--   message_delta → final usage info → StreamUsage
--   message_stop → StreamDone
--
--   Claude's thinking events map to StreamThinkingDelta
instance Provider ClaudeProvider where

    generate :: (IOE :> es, Error ProviderError :> es)
             => ClaudeProvider
             -> ProviderConfig
             -> GenerationConfig
             -> [Message]
             -> [ToolDefinition]
             -> Eff es CompletionResponse
    generate _provider _config _genConfig _messages _tools =
        error "Claude.generate: not yet implemented"
    -- TODO: Implement using Claude.V1.Messages.createMessage
    --
    -- Pseudocode:
    --   -- Extract system messages (Claude requires them as a top-level param)
    --   let (systemBlocks, nonSystemMessages) = partitionSystemMessages messages
    --   let claudeMessages = map toClaudeMessage nonSystemMessages
    --   let claudeTools = map toClaudeTool tools
    --   let request = _CreateMessage
    --         { messages = claudeMessages
    --         , model = gcModel genConfig
    --         , max_tokens = fromMaybe 4096 (gcMaxTokens genConfig)
    --         , tools = nonEmpty claudeTools
    --         , system = nonEmpty systemBlocks
    --         , temperature = gcTemperature genConfig
    --         , top_p = gcTopP genConfig
    --         , stop_sequences = gcStopSequences genConfig
    --         }
    --   response <- liftIO $ createMessage methods request
    --   pure $ fromClaudeResponse response

    stream :: (IOE :> es, Error ProviderError :> es)
           => ClaudeProvider
           -> ProviderConfig
           -> GenerationConfig
           -> [Message]
           -> [ToolDefinition]
           -> Eff es (ConduitT () StreamEvent (Eff es) ())
    stream _provider _config _genConfig _messages _tools =
        error "Claude.stream: not yet implemented"
    -- TODO: Implement using Claude.V1.Messages streaming support
    --
    -- Pseudocode:
    --   Create a conduit source that:
    --   1. Opens a streaming connection to Claude's Messages API
    --   2. Parses SSE events as they arrive
    --   3. Converts each event to a StreamEvent:
    --      - content_block_delta (text) → StreamTextDelta
    --      - content_block_start (tool_use) → StreamToolCallStart
    --      - content_block_delta (input_json_delta) → StreamToolCallDelta
    --      - content_block_stop (tool_use) → StreamToolCallEnd
    --      - thinking_delta → StreamThinkingDelta
    --      - message_delta (usage) → StreamUsage
    --      - message_stop → StreamDone
    --   4. Yields StreamEvents through the conduit
    --   5. Yields StreamDone when the stream ends

    respond :: (IOE :> es, Error ProviderError :> es)
            => ClaudeProvider
            -> ProviderConfig
            -> GenerationConfig
            -> [Message]
            -> [ToolDefinition]
            -> Eff es CompletionResponse
    respond _provider _config _genConfig _messages _tools =
        error "Claude.respond: not yet implemented"
    -- TODO: Implement using Claude.V1.Messages.createMessage with tools
    --
    -- For Claude, respond and generate both use the same Messages API
    -- endpoint. The difference is that respond explicitly enables tool
    -- calling by including tool definitions in the request.
    --
    -- The respond method should:
    -- 1. Always include tool definitions in the request
    -- 2. Handle the tool_use response content blocks
    -- 3. Return a CompletionResponse that may contain ToolCallContent
    --
    -- Pseudocode: same as generate but ensures tools are always sent