{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Agents.Schema
    ( JsonSchema(..)
    , HasJsonSchema(..)
    ) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Key as Key
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics

data JsonSchema = JsonSchema
    { jsSchema  :: Value
    , jsTypeName :: Maybe Text
    , jsStrict   :: Bool
    } deriving (Show, Eq)

-- | Automatically derive a JSON Schema from a Haskell type.
--
-- For record types, each field becomes a property in the schema.
-- Fields wrapped in @Maybe@ are omitted from the \"required\" list.
--
-- === Defining tools with auto-derived schemas
--
-- @
-- data GetWeatherArgs = GetWeatherArgs
--     { city :: Text
--     , unit :: Maybe Text
--     } deriving (Generic, FromJSON, HasJsonSchema)
--
-- weatherTool :: ToolHandler
-- weatherTool = tool \"get_weather\" \"Get the current weather\" $ \\args -> do
--     pure $ Aeson.object [\"temperature\" Aeson..= (22 :: Int), ...]
-- @
--
-- === Primitive type mappings
--
-- @
-- jsonSchema \@Text    → {"type": "string"}
-- jsonSchema \@Int     → {"type": "integer"}
-- jsonSchema \@Double  → {"type": "number"}
-- jsonSchema \@Bool    → {"type": "boolean"}
-- jsonSchema \@[a]    → {"type": "array", "items": jsonSchema \@a}
-- @
class HasJsonSchema a where
    jsonSchema :: Value
    default jsonSchema :: (Generic a, GRecordSchema (Rep a)) => Value
    jsonSchema =
        let (props, req) = gRecordSchema @(Rep a)
        in object
            [ "type" .= ("object" :: Text)
            , "properties" .= object (map (\(k, v) -> Key.fromText k .= v) props)
            , "required" .= Vector.fromList req
            ]

-- Primitive type instances

instance HasJsonSchema Text where
    jsonSchema = object ["type" .= ("string" :: Text)]

instance HasJsonSchema String where
    jsonSchema = object ["type" .= ("string" :: Text)]

instance HasJsonSchema Char where
    jsonSchema = object ["type" .= ("string" :: Text)]

instance HasJsonSchema Int where
    jsonSchema = object ["type" .= ("integer" :: Text)]

instance HasJsonSchema Integer where
    jsonSchema = object ["type" .= ("integer" :: Text)]

instance HasJsonSchema Int8 where
    jsonSchema = object ["type" .= ("integer" :: Text)]

instance HasJsonSchema Int16 where
    jsonSchema = object ["type" .= ("integer" :: Text)]

instance HasJsonSchema Int32 where
    jsonSchema = object ["type" .= ("integer" :: Text)]

instance HasJsonSchema Int64 where
    jsonSchema = object ["type" .= ("integer" :: Text)]

instance HasJsonSchema Word where
    jsonSchema = object ["type" .= ("integer" :: Text)]

instance HasJsonSchema Word8 where
    jsonSchema = object ["type" .= ("integer" :: Text)]

instance HasJsonSchema Word16 where
    jsonSchema = object ["type" .= ("integer" :: Text)]

instance HasJsonSchema Word32 where
    jsonSchema = object ["type" .= ("integer" :: Text)]

instance HasJsonSchema Word64 where
    jsonSchema = object ["type" .= ("integer" :: Text)]

instance HasJsonSchema Double where
    jsonSchema = object ["type" .= ("number" :: Text)]

instance HasJsonSchema Float where
    jsonSchema = object ["type" .= ("number" :: Text)]

instance HasJsonSchema Bool where
    jsonSchema = object ["type" .= ("boolean" :: Text)]

instance HasJsonSchema a => HasJsonSchema [a] where
    jsonSchema = object
        [ "type" .= ("array" :: Text)
        , "items" .= jsonSchema @a
        ]

-- | @Maybe a@ uses the inner type's schema.
-- Optionality is handled in record derivation: @Maybe@ fields
-- are excluded from the \"required\" list.
instance HasJsonSchema a => HasJsonSchema (Maybe a) where
    jsonSchema = jsonSchema @a

-- Generic record schema derivation

-- | Extract field names and schemas from a Generic representation.
--   Returns @(properties, required)@ where properties is a list
--   of @(name, schema)@ pairs and required is a list of field names
--   that are mandatory (non-Maybe).
class GRecordSchema f where
    gRecordSchema :: ([(Text, Value)], [Text])

-- Data type wrapper (M1 D)
instance GRecordSchema f => GRecordSchema (M1 D d f) where
    gRecordSchema = gRecordSchema @f

-- Constructor wrapper (M1 C)
instance GRecordSchema f => GRecordSchema (M1 C c f) where
    gRecordSchema = gRecordSchema @f

-- Product type: combine fields from both sides
instance (GRecordSchema f, GRecordSchema g) => GRecordSchema (f :*: g) where
    gRecordSchema =
        let (fProps, fReq) = gRecordSchema @f
            (gProps, gReq) = gRecordSchema @g
        in (fProps ++ gProps, fReq ++ gReq)

-- Unit type (empty record)
instance GRecordSchema U1 where
    gRecordSchema = ([], [])

-- Maybe field: in properties but NOT in required
instance {-# OVERLAPPING #-} (Selector s, HasJsonSchema a) => GRecordSchema (S1 s (K1 R (Maybe a))) where
    gRecordSchema =
        let name = Text.pack (selName (undefined :: M1 S s (K1 R (Maybe a)) ()))
        in ([(name, jsonSchema @a)], [])

-- Required field: in properties AND in required
instance {-# OVERLAPPABLE #-} (Selector s, HasJsonSchema a) => GRecordSchema (S1 s (K1 R a)) where
    gRecordSchema =
        let name = Text.pack (selName (undefined :: M1 S s (K1 R a) ()))
        in ([(name, jsonSchema @a)], [name])