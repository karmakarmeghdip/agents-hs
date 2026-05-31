module Agents.Schema
    ( JsonSchema(..)
    , jsonSchema
    ) where

import Data.Aeson (Value)
import Data.Text (Text)

-- | A JSON Schema representation for structured output enforcement.
--
-- === Architecture Decision: Generic/Aeson-deriving
--
-- Rather than hand-write JSON Schema @Value@s, we use GHC's Generic
-- mechanism to automatically derive both:
--
-- 1. The JSON Schema (via 'jsonSchema') - for telling the LLM what
--    shape of data to return
-- 2. The ToJSON/FromJSON instances - for parsing the LLM's response
--
-- This gives us type-safe roundtripping: a Haskell type T can be
-- turned into a schema (for the LLM request) and the LLM's JSON
-- response can be parsed back into T (for the application logic).
--
-- === Usage Example
--
-- @
-- {\-# LANGUAGE DeriveGeneric, DeriveAnyClass #-\}
--
-- data WeatherResponse = WeatherResponse
--     { temperature :: Double
--     , condition   :: Text
--     , humidity    :: Maybe Double
--     } deriving (Generic, ToJSON, FromJSON)
--
-- -- Derive the JSON Schema for WeatherResponse:
-- schema :: Value
-- schema = jsonSchema \@WeatherResponse
--
-- -- Use in GenerationConfig:
-- config = defaultGenerationConfig
--     { gcResponseSchema = Just schema }
-- @
--
-- === Implementation Plan
--
-- The 'jsonSchema' function inspects the Generic representation of a
-- type and produces a JSON Schema @Value@. It handles:
--
-- * Basic types: Text/String → {\"type\": \"string\"}
-- * Numeric types: Int, Double → {\"type\": \"integer\"/\"number\"}
-- * Maybe a → makes the field optional (not in \"required\")
-- * Lists → {\"type\": \"array\", \"items\": ...}
-- * Records → {\"type\": \"object\", \"properties\": ..., \"required\": ...}
-- * Sum types → {\"oneOf\": [...]} or enumerated strings
--
-- This is similar to how the @aeson@ package derives ToJSON/FromJSON,
-- but producing JSON Schema instead of JSON instances.
--
-- === Provider Mapping
--
-- * OpenAI: The schema is passed via @response_format.type = \"json_schema\@
--   with @response_format.json_schema.schema@ set to the derived schema.
-- * Claude: The schema is passed via tool definitions or via the
--   structured output feature with @response_format@.
--
-- Both providers require JSON Schema Draft 2020-12 or similar.
data JsonSchema = JsonSchema
    { jsSchema    :: Value
    -- ^ The actual JSON Schema as an Aeson Value.
    --   This is what gets sent to the LLM provider.

    , jsTypeName   :: Maybe Text
    -- ^ Optional name for the schema. Required by some providers
    --   (e.g., OpenAI's structured outputs require a name).
    --   If Nothing, a default name will be derived from the type.

    , jsStrict     :: Bool
    -- ^ Whether to enforce strict schema adherence.
    --   When True, the provider must generate output that exactly
    --   conforms to the schema (no additional properties allowed).
    --   Some providers support this (OpenAI's strict mode) while
    --   others may ignore it.
    } deriving (Show, Eq)

-- | Derive a JSON Schema from a Haskell type.
--
-- This is the primary entry point for structured output support.
-- Given a type @a@ that has a @Generic@ instance, it produces
-- a JSON Schema @Value@ that can be passed to LLM providers.
--
-- === Implementation Plan
--
-- Use @GHC.Generics@ to walk the type's structure and produce
-- a JSON Schema. The key cases:
--
-- @
-- jsonSchema \@Text       → {"type": "string"}
-- jsonSchema \@Int        → {"type": "integer"}
-- jsonSchema \@Double     → {"type": "number"}
-- jsonSchema \@Bool       → {"type": "boolean"}
-- jsonSchema @(Maybe a)   → jsonSchema \@a  (but field is not required)
-- jsonSchema @"[a]"       → {"type": "array", "items": jsonSchema \@a}
-- jsonSchema \@MyRecord  → {"type": "object", "properties": {...}, "required": [...]}
-- @
--
-- For record types, each field becomes a property. Fields wrapped
-- in @Maybe@ are omitted from the \"required\" list.
--
-- This function has the signature:
--
-- @
-- jsonSchema :: forall a. (HasJsonSchema a) => Value
-- @
--
-- where @HasJsonSchema@ is a typeclass that can be derived via
-- @Generic@ for standard types.
--
-- === Future Work
--
-- * Support for @newtype@ wrappers (transparent in schema)
-- * Support for custom JSON Schema annotations (description, examples)
-- * Support for @oneOf@/@anyOf@/@allOf@ for sum types
-- * Integration with @typed-json-schema@ or similar packages
jsonSchema :: forall a. Value
jsonSchema = error "jsonSchema: not yet implemented. Use GHC.Generics to derive JSON Schema from Haskell types."
-- TODO: Implement using GHC.Generics.
-- The HasJsonSchema typeclass and its Generic-derived instances
-- will walk the type structure and produce a JSON Schema Value.
-- This should handle: basic types, Maybe, lists, records, and
-- simple sum types.