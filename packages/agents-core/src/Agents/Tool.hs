{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Agents.Tool
    ( ToolDefinition(..)
    , ToolHandler(..)
    , ToolRegistry
    , toolHandlerDef
    , toolRegistryDefs
    , tool
    ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Aeson (Value, FromJSON)

import Agents.Schema (HasJsonSchema(..))

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

-- | A typed tool handler that pairs a tool definition with a Haskell function.
--
-- === Architecture Decision: Existential Type
--
-- @ToolHandler@ uses an existential type to hide the concrete argument type.
-- This allows storing handlers for different tools (with different argument
-- types) in the same collection ('ToolRegistry').
--
-- The 'FromJSON' constraint ensures the agent loop can automatically parse
-- the LLM's JSON arguments into the correct Haskell type before passing
-- them to the handler function.
--
-- === Type Safety
--
-- When the agent loop encounters a tool call:
--
-- 1. It looks up the handler by tool name in the 'ToolRegistry'
-- 2. It parses the raw JSON arguments using 'Data.Aeson.fromJSON'
--    (leveraging the 'FromJSON' constraint captured in the existential)
-- 3. If parsing succeeds, it calls the handler with the typed arguments
-- 4. If parsing fails, it creates an error 'Agents.Types.ToolResult'
--    and feeds it back to the LLM
--
-- === Creating ToolHandlers
--
-- Use the 'tool' smart constructor for the most ergonomic API:
--
-- @
-- data WeatherArgs = WeatherArgs
--     { city :: Text
--     , unit :: Maybe Text
--     } deriving (Generic, FromJSON, HasJsonSchema)
--
-- weatherHandler :: ToolHandler
-- weatherHandler = tool \"get_weather\" \"Get the current weather for a city\" $ \\args -> do
--     pure $ Aeson.object [\"temperature\" Aeson..= (22 :: Int)]
-- @
--
-- The 'tool' function automatically:
--
-- * Derives the JSON Schema from @WeatherArgs@ via 'HasJsonSchema'
-- * Creates the 'ToolDefinition' with name, description, and schema
-- * Wraps everything in a 'ToolHandler' existential
data ToolHandler where
    ToolHandler :: FromJSON a => ToolDefinition -> (a -> IO Value) -> ToolHandler

-- | Extract the tool definition from a handler.
--
-- Useful for collecting all tool definitions to send to the LLM.
toolHandlerDef :: ToolHandler -> ToolDefinition
toolHandlerDef (ToolHandler def _) = def

-- | A registry mapping tool names to their handlers.
--
-- The 'ToolRegistry' is passed to 'Agents.Agent.Agent' and used by
-- the agent loop to look up and execute tool calls from the LLM.
--
-- Build a registry using 'Data.Map.fromList':
--
-- @
-- registry <- Map.fromList
--     [ ("get_weather", weatherHandler)
--     , ("search", searchHandler)
--     ]
-- @
type ToolRegistry = Map Text ToolHandler

-- | Extract all tool definitions from a registry.
--
-- Used internally by the agent loop to build the list of tool
-- definitions to send to the LLM in each generation call.
toolRegistryDefs :: ToolRegistry -> [ToolDefinition]
toolRegistryDefs = map toolHandlerDef . Map.elems

-- | Smart constructor for creating a 'ToolHandler' with auto-derived JSON Schema.
--
-- This is the recommended way to define tools. It combines the
-- 'ToolDefinition' creation (with auto-derived schema) and the
-- handler function into a single call:
--
-- @
-- data CalculatorArgs = CalculatorArgs
--     { expression :: Text
--     } deriving (Generic, FromJSON, HasJsonSchema)
--
-- calcTool :: ToolHandler
-- calcTool = tool \"calculator\" \"Evaluate a math expression\" $ \\CalculatorArgs{..} -> do
--     pure $ Aeson.object [\"result\" Aeson..= (42 :: Int)]
-- @
--
-- The JSON Schema for the tool parameters is automatically derived
-- from the argument type via 'HasJsonSchema'. Fields wrapped in
-- @Maybe@ are marked as optional (excluded from the \"required\" list).
tool :: forall a. (HasJsonSchema a, FromJSON a) => Text -> Text -> (a -> IO Value) -> ToolHandler
tool name desc handler = ToolHandler
    (ToolDefinition
        { tdName        = name
        , tdDescription = desc
        , tdParameters  = jsonSchema @a
        })
    handler