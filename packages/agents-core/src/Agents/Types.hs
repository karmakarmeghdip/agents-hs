module Agents.Types
    ( -- * Message types
      Message(..)
    , MessageRole(..)
    , ContentBlock(..)
      -- * Provider-agnostic response types
    , CompletionResponse(..)
    , UsageInfo(..)
      -- * Generation parameters (per-call)
    , GenerationConfig(..)
      -- * Provider configuration (per-instance)
    , ProviderConfig(..)
      -- * Tool call response types
    , ToolCall(..)
    , ToolResult(..)
    ) where

import Data.Text (Text)
import Data.Aeson (Value)

-- | The role of a message in a conversation.
--
-- === Provider Mapping
--
-- Each provider adapter converts between this and the provider's native
-- role types:
--
-- * @System@ → OpenAI's @system@ role, Claude's system block
-- * @User@ → OpenAI's @user@ role, Claude's @user@ role
-- * @Assistant@ → OpenAI's @assistant@ role, Claude's @assistant@ role
-- * @Tool@ → OpenAI's @tool@ role, Claude's @result@ role
--   (used for sending tool execution results back to the LLM)
data MessageRole
    = System
    -- ^ System prompt / instructions.
    | User
    -- ^ User input message.
    | Assistant
    -- ^ LLM-generated response message.
    | Tool
    -- ^ Tool result message (response to a tool call).
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | A single content block within a message.
--
-- === Design Philosophy
--
-- Following modern LLM API conventions, messages contain a /list/ of
-- content blocks rather than a single string. This allows mixed content:
--
-- * Text + images in a single user message
-- * Text + tool calls in a single assistant message
-- * Multiple tool results in a single tool message
--
-- === Escape Hatch
--
-- The 'EscapeHatchContent' constructor allows embedding provider-specific
-- content that doesn't map to our agnostic types. This follows the
-- \"agnostic + escape hatch\" design: you can use provider-specific features
-- (like OpenAI's @audio_url@ content, or Claude's @cache_control@ hints)
-- without waiting for the framework to add explicit support.
--
-- The 'EscapeHatchContent' carries a raw 'Value' (Aeson JSON) which is
-- the provider-native JSON representation of that content block.
-- Provider adapters must pass this through as-is when converting messages
-- to the provider's native format.
data ContentBlock
    = TextContent Text
    -- ^ Plain text content.

    | ImageContent Text Text
    -- ^ Image content.
    --   Fields: base64-encoded image data, media type (e.g., \"image/png\").
    --   Future: consider adding URL-based images as a separate constructor.

    | ToolCallContent ToolCall
    -- ^ A tool call from the LLM. Embedded in assistant messages.
    --   When the LLM decides to call a tool, it produces one or more
    --   ToolCall values inside its response.

    | ToolResultContent ToolResult
    -- ^ A tool execution result. Embedded in tool-role messages.
    --   After the agent code executes a tool, it constructs a
    --   ToolResult and sends it back as a tool-role message.

    | EscapeHatchContent Value
    -- ^ Escape hatch for provider-specific content blocks.
    --   The Value is the provider-native JSON representation.
    --   This allows advanced users to access provider-specific features
    --   that the framework doesn't yet model explicitly.
    deriving (Show, Eq)

-- | A single message in a conversation.
--
-- === Design Notes
--
-- Each message has a role and a list of content blocks.
-- The list-of-blocks design follows modern LLM APIs (OpenAI, Claude)
-- where a single message can contain multiple types of content.
--
-- === Conventions
--
-- * @System@ messages should typically have a single @TextContent@ block
-- * @User@ messages can mix @TextContent@ and @ImageContent@
-- * @Assistant@ messages can contain @TextContent@ and @ToolCallContent@
-- * @Tool@ messages should contain @ToolResultContent@
data Message = Message
    { messageRole    :: MessageRole
    , messageContent :: [ContentBlock]
    } deriving (Show, Eq)

-- | Configuration for a specific generation call.
--
-- === Architecture Decision: Config Per-Call Split
--
-- Generation parameters (model, temperature, etc.) live in this
-- per-call config, SEPARATE from 'ProviderConfig' which holds
-- connection details. This follows the AI SDK pattern where:
--
-- * @ProviderConfig@ is created once and reused (API key, base URL)
-- * @GenerationConfig@ varies per call (model, temperature, etc.)
--
-- This split enables:
--
-- * Using different models for different tasks within the same session
-- * A/B testing with different parameters
-- * Keeping auth config separate from generation behavior
data GenerationConfig = GenerationConfig
    { gcModel           :: Text
    -- ^ The model identifier (e.g., \"gpt-4o\", \"claude-sonnet-4-5-20250929\").
    --   REQUIRED: This must always be specified per-call.

    , gcMaxTokens       :: Maybe Int
    -- ^ Maximum tokens to generate. Provider-specific default if Nothing.
    --   Maps to OpenAI's max_tokens and Claude's max_tokens.

    , gcTemperature     :: Maybe Double
    -- ^ Sampling temperature (0.0 - 2.0). Provider-specific default if Nothing.
    --   Higher values = more random, lower = more deterministic.

    , gcTopP            :: Maybe Double
    -- ^ Nucleus sampling parameter. Provider-specific default if Nothing.

    , gcStopSequences   :: Maybe [Text]
    -- ^ Sequences where the LLM should stop generating.
    --   Maps to OpenAI's stop and Claude's stop_sequences.

    , gcResponseSchema  :: Maybe Value
    -- ^ JSON Schema for structured output enforcement.
    --   When provided, the provider must return valid JSON conforming
    --   to this schema. See 'Agents.Schema' for deriving schemas from
    --   Haskell types using Generic.
    --   Maps to OpenAI's response_format with json_schema type,
    --   and Claude's tool_use with structured output.
    } deriving (Show, Eq)

-- | Static provider configuration (created once per provider instance).
--
-- === Architecture Decision: Provider Config vs Generation Config
--
-- This config holds authentication and connection details that don't
-- change between calls. It's the \"how to connect\" config, while
-- 'GenerationConfig' is the \"what to generate\" config.
--
-- === Future Considerations
--
-- * Add retry policy configuration (max retries, backoff strategy)
-- * Add timeout defaults
-- * Add logging/observability configuration
-- * Consider adding a custom HTTP client manager for connection pooling
data ProviderConfig = ProviderConfig
    { pcApiKey  :: Text
    -- ^ API key for authentication. REQUIRED.

    , pcBaseUrl :: Text
    -- ^ Base URL for the provider's API endpoint.
    --   Set to empty string (\"\") to use the provider's default endpoint.
    --   Useful for: proxy servers, enterprise deployments, local LLMs.
    --   Maps to the base URL parameter in both openai and claude packages.
    } deriving (Show, Eq)

-- | A tool call returned by the LLM.
--
-- === Architecture Decision: Simple ToolCall Record
--
-- We keep this as a simple record with just the essential fields.
-- This is the \"what the LLM requested\" data, NOT a full lifecycle model.
-- The agent loop (in a future higher-level package) will:
--
-- 1. Receive @ToolCallContent msg@ in the assistant's response
-- 2. Look up the tool by @tcName@
-- 3. Execute the tool, producing a @ToolResult@
-- 4. Send @ToolResultContent@ back as a tool-role message
--
-- The @tcId@ field is required by both OpenAI and Claude APIs to
-- match tool results back to their corresponding tool calls.
data ToolCall = ToolCall
    { tcId        :: Text
    -- ^ Unique identifier for this tool call, assigned by the LLM.
    --   Must be included in the corresponding 'ToolResult' as @trToolCallId@.

    , tcName      :: Text
    -- ^ The name of the tool the LLM is requesting to call.
    --   Must match one of the 'ToolDefinition' names provided in the request.

    , tcArguments :: Value
    -- ^ The arguments for the tool call as a JSON Value.
    --   Parsed from the LLM's JSON output.
    --   May need validation against the tool's parameter schema.
    --   Stored as raw Value for maximum flexibility.
    } deriving (Show, Eq)

-- | The result of executing a tool call.
--
-- This is what the agent code produces after executing a tool.
-- It gets sent back to the LLM as a tool-role message containing
-- @ToolResultContent@.
data ToolResult = ToolResult
    { trToolCallId :: Text
    -- ^ The ID of the tool call this result corresponds to.
    --   Must match the @tcId@ of the 'ToolCall' being responded to.

    , trResult     :: Value
    -- ^ The tool's output as a JSON Value.

    , trIsError    :: Bool
    -- ^ Whether the tool execution resulted in an error.
    --   If True, @trResult@ should contain error details.
    --   Both OpenAI and Claude have explicit error handling for tool results.
    } deriving (Show, Eq)

-- | Response from a non-streaming generation call.
--
-- === Design Notes
--
-- The completion response includes both the LLM's output and metadata.
-- We track token usage separately so agents can monitor costs and
-- enforce token budgets.
--
-- The @crFinishReason@ field uses 'Text' rather than a sum type because
-- providers use different reason strings and we want to be extensible
-- without breaking changes. Common values: @\"stop\"@, @\"tool_calls\"@,
-- @\"length\"@, @\"content_filter\"@.
data CompletionResponse = CompletionResponse
    { crContent      :: [ContentBlock]
    -- ^ The generated content blocks (text, tool calls, etc.)

    , crModel        :: Text
    -- ^ The actual model used for generation.
    --   May differ from the requested model if the provider
    --   maps aliases (e.g., \"gpt-4\" → \"gpt-4-0613\").

    , crFinishReason :: Text
    -- ^ Why generation stopped. Common values:
    --   \"stop\" - natural end, \"tool_calls\" - LLM wants to call tools,
    --   \"length\" - hit max_tokens, \"content_filter\" - filtered.

    , crUsage        :: UsageInfo
    -- ^ Token usage statistics for cost tracking and budgeting.
    } deriving (Show, Eq)

-- | Token usage information for tracking costs.
--
-- Both OpenAI and Claude report prompt and completion token counts.
-- We normalize them into a single type. Claude also reports
-- cache hit/miss tokens - those could be added as extensions
-- via the escape hatch in the future.
data UsageInfo = UsageInfo
    { uiPromptTokens     :: Int
    -- ^ Number of tokens in the prompt/input.

    , uiCompletionTokens :: Int
    -- ^ Number of tokens in the completion/output.

    , uiTotalTokens      :: Int
    -- ^ Total tokens (prompt + completion).
    } deriving (Show, Eq)