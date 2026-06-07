{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Agents.Provider.Claude.Streaming
    ( ClaudeAccumState(..)
    , emptyClaudeAccumState
    , processStreamEvent
    , finalizeClaudeAccumState
    ) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IORef (IORef, readIORef, writeIORef)
import Numeric.Natural (Natural)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE

import qualified Claude.V1.Messages as CM

import Agents.StreamEvent (StreamEvent(..))

data ClaudeToolCallAccum = ClaudeToolCallAccum
    { ctcaId        :: !Text
    , ctcaName      :: !Text
    , ctcaArguments :: !Text
    }

data ClaudeAccumState = ClaudeAccumState
    { casToolCalls    :: !(IntMap ClaudeToolCallAccum)
    , casInputTokens  :: !Int
    }

emptyClaudeAccumState :: ClaudeAccumState
emptyClaudeAccumState = ClaudeAccumState
    { casToolCalls = IntMap.empty
    , casInputTokens = 0
    }

processStreamEvent :: IORef ClaudeAccumState -> CM.MessageStreamEvent -> IO [StreamEvent]
processStreamEvent accumRef = \case
    CM.Message_Start{CM.message = msg} -> do
        let u = msg.usage
            inputTokens = fromIntegral (u.input_tokens)
        writeIORef accumRef emptyClaudeAccumState { casInputTokens = inputTokens }
        pure []
    CM.Content_Block_Start{CM.index = idx, CM.content_block = cb} -> do
        accum <- readIORef accumRef
        let (events, newAccum) = handleBlockStart accum idx cb
        writeIORef accumRef newAccum
        pure events
    CM.Content_Block_Delta{CM.index = idx, CM.delta = delta} -> do
        accum <- readIORef accumRef
        let (events, newAccum) = handleBlockDelta accum idx delta
        writeIORef accumRef newAccum
        pure events
    CM.Content_Block_Stop{CM.index = idx} -> do
        accum <- readIORef accumRef
        let (events, newAccum) = handleBlockStop accum idx
        writeIORef accumRef newAccum
        pure events
    CM.Message_Delta{CM.usage = su} -> do
        accum <- readIORef accumRef
        let inputTokens = casInputTokens accum
            outputTokens = fromIntegral (su.output_tokens)
        pure [StreamUsage inputTokens outputTokens (inputTokens + outputTokens)]
    CM.Message_Stop -> pure []
    CM.Ping -> pure []
    CM.Error{CM.error = v} -> pure [StreamError (Text.pack (show v))]

handleBlockStart :: ClaudeAccumState -> Natural -> CM.ContentBlock -> ([StreamEvent], ClaudeAccumState)
handleBlockStart accum idx = \case
    CM.ContentBlock_Tool_Use{CM.id = tid, CM.name = tname} ->
        let newAccum = accum { casToolCalls = IntMap.insert (fromIntegral idx) ClaudeToolCallAccum
                                    { ctcaId = tid
                                    , ctcaName = tname
                                    , ctcaArguments = ""
                                    }
                                    (casToolCalls accum)
                           }
        in ([StreamToolCallStart tid tname], newAccum)
    CM.ContentBlock_Text{} -> ([], accum)
    CM.ContentBlock_Thinking{} -> ([], accum)
    CM.ContentBlock_Redacted_Thinking{} -> ([], accum)
    CM.ContentBlock_Unknown{} -> ([], accum)
    CM.ContentBlock_Server_Tool_Use{} -> ([], accum)
    CM.ContentBlock_Tool_Search_Tool_Result{} -> ([], accum)
    CM.ContentBlock_Code_Execution_Tool_Result{} -> ([], accum)

parsePartialJson :: Text -> Aeson.Value
parsePartialJson txt
    | Text.null txt = Aeson.object []
    | otherwise     = case Aeson.decode (LBS.fromStrict (TE.encodeUtf8 txt)) of
        Just v  -> v
        Nothing -> Aeson.toJSON txt

handleBlockDelta :: ClaudeAccumState -> Natural -> CM.ContentBlockDelta -> ([StreamEvent], ClaudeAccumState)
handleBlockDelta accum idx = \case
    CM.Delta_Text_Delta{CM.text = t}
        | Text.null t -> ([], accum)
        | otherwise -> ([StreamTextDelta t], accum)
    CM.Delta_Input_Json_Delta{CM.partial_json = fragment} ->
        case IntMap.lookup (fromIntegral idx) (casToolCalls accum) of
            Just tc ->
                let newArgs = ctcaArguments tc <> fragment
                    newAccum = accum { casToolCalls = IntMap.insert (fromIntegral idx) tc { ctcaArguments = newArgs } (casToolCalls accum) }
                in ([StreamToolCallDelta (ctcaId tc) (parsePartialJson newArgs)], newAccum)
            Nothing -> ([], accum)
    CM.Delta_Thinking_Delta{CM.thinking = t}
        | Text.null t -> ([], accum)
        | otherwise -> ([StreamThinkingDelta t], accum)
    CM.Delta_Signature_Delta{} -> ([], accum)

handleBlockStop :: ClaudeAccumState -> Natural -> ([StreamEvent], ClaudeAccumState)
handleBlockStop accum idx =
    case IntMap.lookup (fromIntegral idx) (casToolCalls accum) of
        Just tc ->
            let parsedArgs = case Aeson.decode (LBS.fromStrict (TE.encodeUtf8 (ctcaArguments tc))) of
                    Just v -> v
                    Nothing -> Aeson.object []
                newAccum = accum { casToolCalls = IntMap.delete (fromIntegral idx) (casToolCalls accum) }
            in ([StreamToolCallEnd (ctcaId tc) (ctcaName tc) parsedArgs], newAccum)
        Nothing -> ([], accum)

finalizeClaudeAccumState :: ClaudeAccumState -> [StreamEvent]
finalizeClaudeAccumState accumState =
    IntMap.foldr finalizeAccum [] (casToolCalls accumState)
  where
    finalizeAccum tc evts =
        let fullArgsText = ctcaArguments tc
            parsedArgs = case Aeson.decode (LBS.fromStrict (TE.encodeUtf8 fullArgsText)) of
                Just v -> v
                Nothing -> Aeson.object []
        in StreamToolCallEnd (ctcaId tc) (ctcaName tc) parsedArgs : evts