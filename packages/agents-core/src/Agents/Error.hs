module Agents.Error
    ( ProviderError(..)
    ) where

import Data.Text (Text)
import Data.Aeson (Value)

-- | Sum type representing all possible errors that can occur
-- when interacting with an LLM provider.
--
-- === Error Handling Strategy
--
-- Errors are thrown via Effectful's @Error ProviderError@ effect.
-- This means callers can use @Effectful.Error.catchError@ or
-- @Effectful.Error.runError@ to handle errors within the @Eff@ monad.
--
-- Each provider implementation wraps its native error types into
-- the appropriate constructor of this type. For example:
--
-- * The OpenAI adapter converts servant-client errors to 'ConnectionError'
-- * The OpenAI adapter converts HTTP 401 to 'AuthenticationError'
-- * The Claude adapter converts Anthropic\'s error responses similarly
--
-- === Usage in the Eff stack
--
-- A typical Provider method signature looks like:
--
-- @
-- generate :: (IOE :> es, Error ProviderError :> es)
--          => p -> ProviderConfig -> GenerationConfig -> [Message] -> [ToolDefinition]
--          -> Eff es CompletionResponse
-- @
--
-- Callers can handle errors with:
--
-- @
-- result <- runEff . runError $ do
--     resp <- generate openaiProvider config genConfig messages tools
--     pure resp
-- case result of
--     Left err  -> handleError err
--     Right resp -> processResponse resp
-- @
--
-- === Future Considerations
--
-- * Add retry-specific errors (rate limited with retry-after, service unavailable)
-- * Add request validation errors (invalid schema, malformed request)
-- * Consider adding a @PartialResult ProviderError a@ constructor for streaming
--   scenarios where we have some data but also an error
data ProviderError
    = AuthenticationError Text
    -- ^ API key is invalid, expired, or missing.
    --   The Text field contains the provider's error message.

    | RateLimitError Text
    -- ^ The provider's rate limit has been exceeded.
    --   Should trigger retry logic in higher-level agent loops.
    --   Future: include retry-after hint.

    | InvalidRequestError Text
    -- ^ The request was malformed or contained invalid parameters.
    --   This includes: invalid model names, unsupported features,
    --   schema validation failures, etc.

    | ProviderAPIError Text Int
    -- ^ A generic API error from the provider.
    --   Fields: error message, HTTP status code.

    | ResponseParseError Text Value
    -- ^ The provider returned a response that could not be parsed
    --   into our agnostic types.
    --   Fields: error message, the raw JSON Value that failed to parse.
    --   The raw Value enables debugging and escape-hatch extraction.

    | ConnectionError Text
    -- ^ Network/transport error when contacting the provider.

    | TimeoutError Text
    -- ^ The request timed out waiting for a response.

    | StructuredOutputError Text
    -- ^ The LLM's response did not conform to the requested
    --   structured output schema (JSON Schema validation failed).

    | ToolCallParseError Text
    -- ^ A tool call response from the LLM could not be parsed.
    --   E.g., the function name or arguments were malformed.

    | ProviderSpecificError Text Value
    -- ^ An error specific to a provider that doesn't fit the
    --   above categories. The Value contains provider-specific
    --   error details for debugging and escape-hatch purposes.
    --   Example: OpenAI's content policy violation, Claude's
    --   prompt caching error.
    deriving (Show, Eq)