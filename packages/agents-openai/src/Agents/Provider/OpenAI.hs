{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Agents.Provider.OpenAI
    ( OpenAIProvider(..)
    , newOpenAIProvider
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, newTChanIO, readTChan, writeTChan)
import Control.Exception (SomeException, try)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit (ConduitT, yield)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as Vector
import Data.Vector (Vector)

import Effectful (Eff, IOE, (:>))
import qualified Effectful as E
import Effectful.Error.Static (Error, throwError)

import Servant.Client (ClientEnv)

import qualified OpenAI.V1 as OpenAI
import qualified OpenAI.V1.Chat.Completions as CC
import qualified OpenAI.V1.Chat.Completions.Stream as CCS
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
    , ProviderConfig(..)
    , ToolCall(..)
    , ToolResult(..)
    )
import Agents.StreamEvent (StreamEvent(..))
import Agents.Tool (ToolDefinition(..))
import Agents.Provider (Provider(..))

data OpenAIProvider = OpenAIProvider
    { oaiClientEnv :: ClientEnv
    , oaiApiKey    :: Text
    , oaiOrg      :: Maybe Text
    , oaiProject  :: Maybe Text
    }

newOpenAIProvider :: ProviderConfig -> IO OpenAIProvider
newOpenAIProvider config = do
    let baseUrlStr = if Text.null (pcBaseUrl config)
                       then "https://api.openai.com"
                       else pcBaseUrl config
    clientEnv <- OpenAI.getClientEnv baseUrlStr
    pure OpenAIProvider
        { oaiClientEnv = clientEnv
        , oaiApiKey    = pcApiKey config
        , oaiOrg       = Nothing
        , oaiProject   = Nothing
        }

makeOAIMethods :: OpenAIProvider -> OpenAI.Methods
makeOAIMethods provider =
    OpenAI.makeMethods
        (oaiClientEnv provider)
        (oaiApiKey provider)
        (oaiOrg provider)
        (oaiProject provider)

toOpenAIMessages :: [Message] -> Either ProviderError (Vector (CC.Message (Vector CC.Content)))
toOpenAIMessages msgs = Vector.fromList <$> traverse toOpenAIMessage msgs

toOpenAIMessage :: Message -> Either ProviderError (CC.Message (Vector CC.Content))
toOpenAIMessage msg = do
    contents <- traverse toOpenAIContent (messageContent msg)
    let vContents = Vector.fromList contents
    case messageRole msg of
        System    -> pure CC.System
            { CC.content = vContents
            , CC.name    = Nothing
            }
        User      -> pure CC.User
            { CC.content = vContents
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
        Tool      -> case messageContent msg of
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
        [TextContent txt]
    CC.Assistant{ CC.assistant_content = Nothing, CC.tool_calls = Just tcs } ->
        Vector.toList (Vector.map fromOpenAIToolCall tcs)
    CC.Assistant{ CC.assistant_content = Just txt, CC.tool_calls = Just tcs } ->
        TextContent txt : Vector.toList (Vector.map fromOpenAIToolCall tcs)
    CC.Assistant{ CC.assistant_content = Nothing, CC.tool_calls = Nothing } ->
        []
    CC.Tool{ CC.content = txt }    -> [TextContent txt]
    CC.User{ CC.content = txt }    -> [TextContent txt]
    CC.System{ CC.content = txt }  -> [TextContent txt]

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

instance Provider OpenAIProvider where

    generate :: (IOE :> es, Error ProviderError :> es)
             => OpenAIProvider
             -> ProviderConfig
             -> GenerationConfig
             -> [Message]
             -> [ToolDefinition]
             -> Eff es CompletionResponse
    generate provider _config genConfig messages tools = do
        oaiMessages <- either throwError pure (toOpenAIMessages messages)
        let mds = makeOAIMethods provider
            request = buildRequest oaiMessages genConfig tools
        result <- E.liftIO $ try @(SomeException) $
            OpenAI.createChatCompletion mds request
        case result of
            Right resp -> pure $ fromOpenAIResponse resp
            Left exc   -> throwError $ ConnectionError (Text.pack (show exc))

    stream :: (IOE :> es, Error ProviderError :> es)
           => OpenAIProvider
           -> ProviderConfig
           -> GenerationConfig
           -> [Message]
           -> [ToolDefinition]
           -> Eff es (ConduitT () StreamEvent (Eff es) ())
    stream provider _config genConfig messages tools = do
        oaiMessages <- either throwError pure (toOpenAIMessages messages)
        let mds    = makeOAIMethods provider
            request = (buildRequest oaiMessages genConfig tools)
                { CC.stream         = Just True
                , CC.stream_options = Just CC._ChatCompletionStreamOptions
                    { CC.include_usage       = Just True
                    , CC.include_obfuscation = Nothing
                    }
                }
        chan <- E.liftIO newTChanIO
        _ <- E.liftIO $ forkIO $ do
            OpenAI.createChatCompletionStreamTyped mds request $ \event -> do
                atomically $ writeTChan chan (Just event)
            atomically $ writeTChan chan Nothing
        pure $ do
            let loop = do
                    me <- E.liftIO $ atomically $ readTChan chan
                    case me of
                        Nothing -> pure ()
                        Just (Left errMsg) -> yield (StreamError errMsg) >> loop
                        Just (Right chunk) -> do
                            mapM_ yield (chunkToStreamEvents chunk)
                            loop
            loop
            yield StreamDone

    respond :: (IOE :> es, Error ProviderError :> es)
            => OpenAIProvider
            -> ProviderConfig
            -> GenerationConfig
            -> [Message]
            -> [ToolDefinition]
            -> Eff es CompletionResponse
    respond = generate

chunkToStreamEvents :: CCS.ChatCompletionChunk -> [StreamEvent]
chunkToStreamEvents chunk =
    case Vector.toList (CCS.choices chunk) of
        [] -> maybe [] (\u -> [fromUsage u]) (CCS.usage chunk)
        cs -> concatMap choiceEvents cs
  where
    choiceEvents c = textEvents (CCS.delta c) ++ toolCallEvents (CCS.delta c) ++ finishEvent c

    textEvents delta = case CCS.delta_content delta of
        Just t | not (Text.null t) -> [StreamTextDelta t]
        _                          -> []

    toolCallEvents delta = case CCS.delta_tool_calls delta of
        Just tcs -> Vector.toList (Vector.imap toolCallToEvent tcs)
        Nothing  -> []

    toolCallToEvent _ tc =
        let tcId   = OToolCall.id (tc :: OToolCall.ToolCall)
            tcName = OToolCall.name (OToolCall.function tc)
        in StreamToolCallStart tcId tcName

    finishEvent c = case CCS.finish_reason c of
        Just fr | fr == "stop" || fr == "length" || fr == "content_filter" || fr == "tool_calls" -> []
        _ -> []

    fromUsage u = StreamUsage
        (fromIntegral (OUsage.prompt_tokens u))
        (fromIntegral (OUsage.completion_tokens u))
        (fromIntegral (OUsage.total_tokens u))