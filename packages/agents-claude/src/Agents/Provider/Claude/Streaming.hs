{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Agents.Provider.Claude.Streaming
    ( processStreamEvent
    , contentBlockStartEvents
    , contentBlockDeltaEvents
    ) where

import Data.IORef (IORef, readIORef, writeIORef)
import qualified Data.Text as Text

import qualified Claude.V1.Messages as CM

import Agents.StreamEvent (StreamEvent(..))

processStreamEvent :: IORef Int -> CM.MessageStreamEvent -> IO [StreamEvent]
processStreamEvent inputTokensRef = \case
    CM.Message_Start{CM.message = msg} -> do
        let u = msg.usage
        writeIORef inputTokensRef (fromIntegral (u.input_tokens))
        pure []
    CM.Content_Block_Start{CM.content_block = cb} -> pure (contentBlockStartEvents cb)
    CM.Content_Block_Delta{CM.delta = delta} -> pure (contentBlockDeltaEvents delta)
    CM.Content_Block_Stop{} -> pure []
    CM.Message_Delta{CM.usage = su} -> do
        inputTokens <- readIORef inputTokensRef
        let outputTokens = fromIntegral (su.output_tokens)
        pure [StreamUsage inputTokens outputTokens (inputTokens + outputTokens)]
    CM.Message_Stop -> pure []
    CM.Ping -> pure []
    CM.Error{CM.error = v} -> pure [StreamError (Text.pack (show v))]

contentBlockStartEvents :: CM.ContentBlock -> [StreamEvent]
contentBlockStartEvents = \case
    CM.ContentBlock_Tool_Use{CM.id = tid, CM.name = tname} ->
        [StreamToolCallStart tid tname]
    CM.ContentBlock_Text{} -> []
    CM.ContentBlock_Thinking{} -> []
    CM.ContentBlock_Redacted_Thinking{} -> []
    CM.ContentBlock_Unknown{} -> []
    CM.ContentBlock_Server_Tool_Use{} -> []
    CM.ContentBlock_Tool_Search_Tool_Result{} -> []
    CM.ContentBlock_Code_Execution_Tool_Result{} -> []

contentBlockDeltaEvents :: CM.ContentBlockDelta -> [StreamEvent]
contentBlockDeltaEvents = \case
    CM.Delta_Text_Delta{CM.text = t} ->
        [StreamTextDelta t | not (Text.null t)]
    CM.Delta_Input_Json_Delta{} -> []
    CM.Delta_Thinking_Delta{CM.thinking = t} ->
        [StreamThinkingDelta t | not (Text.null t)]
    CM.Delta_Signature_Delta{} -> []
