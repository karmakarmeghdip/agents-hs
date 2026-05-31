module Agents.Tool
    ( ToolDefinition(..)
    ) where

import Data.Text (Text)
import Data.Aeson (Value)

-- | Declarative definition of a tool that can be offered to the LLM.
--
-- === Architecture Decision: Both Layers
--
-- This module defines the __first layer__: declarative tool schemas.
-- This is the \"what tools are available\" description that gets sent
-- to the LLM as part of the request. It contains the tool's name,
-- description, and the JSON Schema for its parameters.
--
-- The __second layer__ (handler dispatch: mapping tool names to
-- Haskell functions that execute them) will be in a future
-- higher-level package. The split is intentional:
--
-- * Layer 1 (this module): Pure data declarations. Language-neutral.
--   Can be serialized to any provider's tool format.
-- * Layer 2 (future): Haskell functions + dispatch. The agent code
--   that actually executes tools.
--
-- === Provider Mapping
--
-- Each provider adapter converts @ToolDefinition@ to its native format:
--
-- * OpenAI: Converts to @OpenAI.V1.Tool@ with type @function@,
--   setting @name@, @description@, and @parameters@ from our fields.
-- * Claude: Converts to @Claude.V1.Tool@ with @name@, @description@,
--   and @input_schema@ from our fields.
--
-- === Schema Derivation
--
-- Instead of writing @Value@ schemas by hand, use 'Agents.Schema.jsonSchema'
-- to auto-derive the JSON Schema from a Haskell type:
--
-- @
-- data GetWeatherArgs = GetWeatherArgs
--     { city :: Text
--     , unit :: Text
--     } deriving (Generic, ToJSON, FromJSON)
--
-- weatherTool :: ToolDefinition
-- weatherTool = ToolDefinition
--     { tdName        = "get_weather"
--     , tdDescription = "Get the current weather for a city"
--     , tdParameters  = jsonSchema @GetWeatherArgs  -- auto-derived
--     }
-- @
data ToolDefinition = ToolDefinition
    { tdName        :: Text
    -- ^ The name of the tool. Must be unique within a single request.
    --   Both OpenAI and Claude require tool names to be unique.

    , tdDescription :: Text
    -- ^ A description of what the tool does. The LLM uses this to
    --   decide when and how to call the tool. Write clear, specific
    --   descriptions for better tool selection.

    , tdParameters  :: Value
    -- ^ JSON Schema describing the tool's input parameters.
    --   This defines the expected structure of @tcArguments@ in
    --   'Agents.Types.ToolCall'.
    --   Use 'Agents.Schema.jsonSchema' to auto-derive this from
    --   a Haskell type, or construct it manually with 'Data.Aeson.object'.
    } deriving (Show, Eq)