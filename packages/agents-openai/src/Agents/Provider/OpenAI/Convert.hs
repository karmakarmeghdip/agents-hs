{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Agents.Provider.OpenAI.Convert
    ( toOpenAIMessages
    , toOpenAIMessage
    , partitionToolCalls
    , toOpenAIContent
    , jsonValueToText
    , toOpenAITools
    , toOpenAITool
    , toResponseFormat
    , toOpenAIStop
    , fromOpenAIResponse
    , fromOpenAIUsage
    , extractContent
    , fromOpenAIMessage
    , filterEmptyText
    , fromOpenAIToolCall
    , buildRequest
    ) where

import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as Vector
import Data.Vector (Vector)

import qualified OpenAI.V1.Chat.Completions as CC
import qualified OpenAI.V1.Tool as OTool
import qualified OpenAI.V1.ToolCall as OToolCall
import qualified OpenAI.V1.ResponseFormat as RF
import qualified OpenAI.V1.Models as OModels
import qualified OpenAI.V1.Usage as OUsage

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

toOpenAIMessages :: [Message] -> Either ProviderError (Vector (CC.Message (Vector CC.Content)))
toOpenAIMessages msgs = Vector.fromList <$> traverse toOpenAIMessage msgs

toOpenAIMessage :: Message -> Either ProviderError (CC.Message (Vector CC.Content))
toOpenAIMessage msg = case messageRole msg of
        System -> do
            contents <- traverse toOpenAIContent (messageContent msg)
            pure CC.System
                { CC.content = Vector.fromList contents
                , CC.name    = Nothing
                }
        User -> do
            contents <- traverse toOpenAIContent (messageContent msg)
            pure CC.User
                { CC.content = Vector.fromList contents
                , CC.name    = Nothing
                }
        Assistant -> case partitionToolCalls (messageContent msg) of
            (textParts, []) -> pure CC.Assistant
                { CC.assistant_content = Just (Vector.fromList textParts)
                , CC.refusal           = Nothing
                , CC.name              = Nothing
                , CC.assistant_audio   = Nothing
                , CC.tool_calls        = Nothing
                }
            (textParts, tcs) -> pure CC.Assistant
                { CC.assistant_content = Just (Vector.fromList textParts)
                , CC.refusal           = Nothing
                , CC.name              = Nothing
                , CC.assistant_audio   = Nothing
                , CC.tool_calls        = Just (Vector.fromList tcs)
                }
        Tool -> case messageContent msg of
            [ToolResultContent tr] -> pure CC.Tool
                { CC.content      = Vector.singleton (CC.Text (jsonValueToText (trResult tr)))
                , CC.tool_call_id = trToolCallId tr
                }
            _ -> Left $ InvalidRequestError "Tool message must contain exactly one ToolResultContent"

partitionToolCalls :: [ContentBlock] -> ([CC.Content], [OToolCall.ToolCall])
partitionToolCalls = foldr go ([], [])
  where
    go (TextContent t) (cs, tcs)  = (CC.Text{CC.text = t} : cs, tcs)
    go (ToolCallContent tc) (cs, tcs) =
        (cs, OToolCall.ToolCall_Function
            { OToolCall.id       = tcId tc
            , OToolCall.function = OToolCall.Function
                { OToolCall.name      = tcName tc
                , OToolCall.arguments = jsonValueToText (tcArguments tc)
                }
            } : tcs)
    go _ acc = acc

toOpenAIContent :: ContentBlock -> Either ProviderError CC.Content
toOpenAIContent (TextContent t) = pure CC.Text{CC.text = t}
toOpenAIContent (ImageContent b64 mt) = pure CC.Image_URL
    { CC.image_url = CC.ImageURL
        { CC.url    = "data:" <> mt <> ";base64," <> b64
        , CC.detail = Nothing
        }
    }
toOpenAIContent (ToolCallContent _)  = Left $ InvalidRequestError
    "ToolCallContent in content list should be handled via message tool_calls field"
toOpenAIContent (ToolResultContent _) = Left $ InvalidRequestError
    "ToolResultContent should only appear in Tool-role messages"
toOpenAIContent (EscapeHatchContent v) = case Aeson.fromJSON v of
    Aeson.Success c -> pure c
    Aeson.Error err -> Left $ ResponseParseError
        ("EscapeHatchContent could not be parsed as OpenAI Content: " <> Text.pack err) v

jsonValueToText :: Value -> Text
jsonValueToText = TE.decodeUtf8 . LBS.toStrict . Aeson.encode

toOpenAITools :: [ToolDefinition] -> Maybe (Vector OTool.Tool)
toOpenAITools [] = Nothing
toOpenAITools tds = Just $ Vector.fromList (map toOpenAITool tds)

toOpenAITool :: ToolDefinition -> OTool.Tool
toOpenAITool td = OTool.Tool_Function OTool.Function
    { OTool.description = Just (tdDescription td)
    , OTool.name        = tdName td
    , OTool.parameters  = Just (tdParameters td)
    , OTool.strict      = Nothing
    }

toResponseFormat :: Maybe Value -> Maybe RF.ResponseFormat
toResponseFormat Nothing   = Nothing
toResponseFormat (Just schema) = Just $ RF.JSON_Schema RF.JSONSchema
    { RF.description = Nothing
    , RF.name        = "response_schema"
    , RF.schema      = Just schema
    , RF.strict     = Just True
    }

toOpenAIStop :: Maybe [Text] -> Maybe (Vector Text)
toOpenAIStop Nothing  = Nothing
toOpenAIStop (Just ss) = Just (Vector.fromList ss)

fromOpenAIResponse :: CC.ChatCompletionObject -> CompletionResponse
fromOpenAIResponse resp = CompletionResponse
    { crContent      = extractContent (CC.choices resp)
    , crModel        = let CC.ChatCompletionObject{ CC.model = m } = resp in OModels.text m
    , crFinishReason = case Vector.toList (CC.choices resp) of
        (c:_) -> CC.finish_reason c
        []    -> "stop"
    , crUsage        = fromOpenAIUsage (CC.usage resp)
    }

fromOpenAIUsage :: OUsage.Usage ct pt -> UsageInfo
fromOpenAIUsage u = UsageInfo
    { uiPromptTokens     = fromIntegral (OUsage.prompt_tokens u)
    , uiCompletionTokens = fromIntegral (OUsage.completion_tokens u)
    , uiTotalTokens      = fromIntegral (OUsage.total_tokens u)
    }

extractContent :: Vector CC.Choice -> [ContentBlock]
extractContent choices = case Vector.toList choices of
    (c:_) -> fromOpenAIMessage (CC.message c)
    []    -> []

fromOpenAIMessage :: CC.Message Text -> [ContentBlock]
fromOpenAIMessage = \case
    CC.Assistant{ CC.assistant_content = Just txt, CC.tool_calls = Nothing } ->
        filterEmptyText [TextContent txt]
    CC.Assistant{ CC.assistant_content = Nothing, CC.tool_calls = Just tcs } ->
        Vector.toList (Vector.map fromOpenAIToolCall tcs)
    CC.Assistant{ CC.assistant_content = Just txt, CC.tool_calls = Just tcs } ->
        filterEmptyText [TextContent txt | not (Text.null txt)]
            ++ Vector.toList (Vector.map fromOpenAIToolCall tcs)
    CC.Assistant{ CC.assistant_content = Nothing, CC.tool_calls = Nothing } ->
        []
    CC.Tool{ CC.content = txt }   -> filterEmptyText [TextContent txt]
    CC.User{ CC.content = txt }   -> filterEmptyText [TextContent txt]
    CC.System{ CC.content = txt } -> filterEmptyText [TextContent txt]

filterEmptyText :: [ContentBlock] -> [ContentBlock]
filterEmptyText = filter (\case TextContent t | Text.null t -> False; _ -> True)

fromOpenAIToolCall :: OToolCall.ToolCall -> ContentBlock
fromOpenAIToolCall tc = ToolCallContent ToolCall
    { tcId   = OToolCall.id (tc :: OToolCall.ToolCall)
    , tcName = OToolCall.name (OToolCall.function tc)
    , tcArguments = case Aeson.decode (LBS.fromStrict (TE.encodeUtf8 (OToolCall.arguments (OToolCall.function tc)))) of
        Just v  -> v
        Nothing -> Aeson.object []
    }

buildRequest :: Vector (CC.Message (Vector CC.Content)) -> GenerationConfig -> [ToolDefinition] -> CC.CreateChatCompletion
buildRequest oaiMessages genConfig tools = CC._CreateChatCompletion
    { CC.messages              = oaiMessages
    , CC.model                 = OModels.Model (gcModel genConfig)
    , CC.max_completion_tokens = fromIntegral <$> gcMaxTokens genConfig
    , CC.temperature           = gcTemperature genConfig
    , CC.top_p                 = gcTopP genConfig
    , CC.stop                  = toOpenAIStop (gcStopSequences genConfig)
    , CC.tools                 = toOpenAITools tools
    , CC.response_format       = toResponseFormat (gcResponseSchema genConfig)
    , CC.store                 = Nothing
    , CC.metadata              = Nothing
    , CC.frequency_penalty     = Nothing
    , CC.logit_bias            = Nothing
    , CC.logprobs              = Nothing
    , CC.top_logprobs          = Nothing
    , CC.n                     = Nothing
    , CC.modalities            = Nothing
    , CC.prediction            = Nothing
    , CC.audio                 = Nothing
    , CC.presence_penalty      = Nothing
    , CC.reasoning_effort      = Nothing
    , CC.seed                   = Nothing
    , CC.service_tier           = Nothing
    , CC.stream                 = Nothing
    , CC.stream_options         = Nothing
    , CC.tool_choice            = Nothing
    , CC.parallel_tool_calls    = Nothing
    , CC.user                   = Nothing
    , CC.web_search_options     = Nothing
    }
