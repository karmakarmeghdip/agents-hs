{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Agents.Provider.Claude.Convert
    ( partitionMessages
    , toClaudeMessages
    , toClaudeMessage
    , toClaudeContent
    , toClaudeToolDefs
    , toClaudeToolDef
    , toClaudeStopSequences
    , toClaudeOutputConfig
    , jsonValueToText
    , buildRequest
    , fromClaudeResponse
    , filterEmptyText
    , fromClaudeContentBlock
    , fromClaudeStopReason
    , fromClaudeUsage
    ) where

import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vector
import Data.Vector (Vector)

import qualified Claude.V1.Messages as CM

import Agents.Error (ProviderError(..))
import Agents.Types
    ( Message(..)
    , MessageRole(..)
    , ContentBlock(..)
    , CompletionResponse(..)
    , UsageInfo(..)
    , GenerationConfig(..)
    , ToolCall(..)
    , ToolResult(..)
    )
import Agents.Tool (ToolDefinition(..))

partitionMessages :: [Message] -> Either ProviderError (Maybe CM.SystemPrompt, [Message])
partitionMessages msgs =
    let (systemTexts, otherMsgs) = foldr go ([], []) msgs
    in case systemTexts of
        [] -> Right (Nothing, otherMsgs)
        ts -> Right (Just (CM.SystemPromptText (Text.intercalate "\n\n" ts)), otherMsgs)
  where
    go msg (sts, rest) = case messageRole msg of
        System -> (extractSystemText msg : sts, rest)
        _      -> (sts, msg : rest)

    extractSystemText msg = Text.intercalate "\n" (map textFromBlock (messageContent msg))

    textFromBlock (TextContent t) = t
    textFromBlock (EscapeHatchContent v) = Text.pack (show v)
    textFromBlock _ = ""

toClaudeMessages :: [Message] -> Either ProviderError (Vector CM.Message)
toClaudeMessages msgs = Vector.fromList <$> traverse toClaudeMessage msgs

toClaudeMessage :: Message -> Either ProviderError CM.Message
toClaudeMessage msg = do
    let role = case messageRole msg of
            User      -> CM.User
            Assistant -> CM.Assistant
            System    -> CM.User
            Tool      -> CM.User
    contents <- traverse toClaudeContent (messageContent msg)
    pure CM.Message
        { CM.role = role
        , CM.content = Vector.fromList contents
        , CM.cache_control = Nothing
        }

toClaudeContent :: ContentBlock -> Either ProviderError CM.Content
toClaudeContent (TextContent t) = Right CM.Content_Text
    { CM.text = t
    , CM.cache_control = Nothing
    }
toClaudeContent (ImageContent b64 mt) = Right CM.Content_Image
    { CM.source = CM.ImageSource
        { CM.type_ = "base64"
        , CM.media_type = mt
        , CM.data_ = b64
        }
    , CM.cache_control = Nothing
    }
toClaudeContent (ToolCallContent tc) = Right CM.Content_Tool_Use
    { CM.id = tcId tc
    , CM.name = tcName tc
    , CM.input = tcArguments tc
    , CM.caller = Nothing
    }
toClaudeContent (ToolResultContent tr) = Right CM.Content_Tool_Result
    { CM.tool_use_id = trToolCallId tr
    , CM.content = Just (jsonValueToText (trResult tr))
    , CM.is_error = Just (trIsError tr)
    }
toClaudeContent (EscapeHatchContent v) = case Aeson.fromJSON v of
    Aeson.Success c -> Right c
    Aeson.Error err -> Left $ ResponseParseError
        ("EscapeHatchContent could not be parsed as Claude Content: " <> Text.pack err) v

toClaudeToolDefs :: [ToolDefinition] -> Maybe (Vector CM.ToolDefinition)
toClaudeToolDefs [] = Nothing
toClaudeToolDefs tds = Just $ Vector.fromList (map toClaudeToolDef tds)

toClaudeToolDef :: ToolDefinition -> CM.ToolDefinition
toClaudeToolDef td = CM.inlineTool (CM.functionTool (tdName td) (Just (tdDescription td)) (tdParameters td))

toClaudeStopSequences :: Maybe [Text] -> Maybe (Vector Text)
toClaudeStopSequences Nothing  = Nothing
toClaudeStopSequences (Just ss) = Just (Vector.fromList ss)

toClaudeOutputConfig :: Maybe Value -> Maybe CM.OutputConfig
toClaudeOutputConfig Nothing   = Nothing
toClaudeOutputConfig (Just schema) = Just (CM.jsonSchemaConfig schema)

jsonValueToText :: Value -> Text
jsonValueToText = TE.decodeUtf8 . LBS.toStrict . Aeson.encode

buildRequest :: Vector CM.Message -> Maybe CM.SystemPrompt -> GenerationConfig -> [ToolDefinition] -> CM.CreateMessage
buildRequest claudeMessages systemPrompt genConfig tools = CM.CreateMessage
    { CM.model = gcModel genConfig
    , CM.messages = claudeMessages
    , CM.max_tokens = fromIntegral (fromMaybe 4096 (gcMaxTokens genConfig))
    , CM.system = systemPrompt
    , CM.cache_control = Nothing
    , CM.temperature = gcTemperature genConfig
    , CM.top_p = gcTopP genConfig
    , CM.top_k = Nothing
    , CM.stop_sequences = toClaudeStopSequences (gcStopSequences genConfig)
    , CM.stream = Nothing
    , CM.metadata = Nothing
    , CM.tools = toClaudeToolDefs tools
    , CM.tool_choice = Nothing
    , CM.container = Nothing
    , CM.context_management = Nothing
    , CM.inference_geo = Nothing
    , CM.speed = Nothing
    , CM.output_config = toClaudeOutputConfig (gcResponseSchema genConfig)
    , CM.thinking = Nothing
    }

fromClaudeResponse :: CM.MessageResponse -> CompletionResponse
fromClaudeResponse resp = CompletionResponse
    { crContent      = filterEmptyText (Vector.toList (Vector.mapMaybe fromClaudeContentBlock (resp.content)))
    , crModel        = resp.model
    , crFinishReason = fromClaudeStopReason (resp.stop_reason)
    , crUsage        = fromClaudeUsage (resp.usage)
    }

filterEmptyText :: [ContentBlock] -> [ContentBlock]
filterEmptyText = filter (\case TextContent t | Text.null t -> False; _ -> True)

fromClaudeContentBlock :: CM.ContentBlock -> Maybe ContentBlock
fromClaudeContentBlock = \case
    CM.ContentBlock_Text{CM.text = t} -> Just (TextContent t)
    CM.ContentBlock_Tool_Use{CM.id = tid, CM.name = tname, CM.input = tinput} ->
        Just (ToolCallContent ToolCall
            { tcId = tid
            , tcName = tname
            , tcArguments = tinput
            })
    CM.ContentBlock_Thinking{CM.thinking = t} -> Just (EscapeHatchContent (Aeson.object ["type" Aeson..= ("thinking" :: Text), "thinking" Aeson..= t]))
    CM.ContentBlock_Redacted_Thinking{} -> Nothing
    CM.ContentBlock_Unknown{CM.raw = v} -> Just (EscapeHatchContent v)
    CM.ContentBlock_Server_Tool_Use{} -> Nothing
    CM.ContentBlock_Tool_Search_Tool_Result{} -> Nothing
    CM.ContentBlock_Code_Execution_Tool_Result{} -> Nothing

fromClaudeStopReason :: Maybe CM.StopReason -> Text
fromClaudeStopReason = \case
    Just CM.End_Turn    -> "stop"
    Just CM.Max_Tokens  -> "length"
    Just CM.Stop_Sequence -> "stop_sequence"
    Just CM.Tool_Use    -> "tool_calls"
    Just CM.Refusal     -> "refusal"
    Just CM.Model_Context_Window_Exceeded -> "length"
    Nothing             -> "stop"

fromClaudeUsage :: CM.Usage -> UsageInfo
fromClaudeUsage u = UsageInfo
    { uiPromptTokens     = fromIntegral (u.input_tokens)
    , uiCompletionTokens = fromIntegral (u.output_tokens)
    , uiTotalTokens      = fromIntegral (u.input_tokens) + fromIntegral (u.output_tokens)
    }
