{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Agents.Provider.OpenAI.Streaming
  ( chunkToStreamEvents,
  )
where

import Agents.StreamEvent (StreamEvent (..))
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import OpenAI.V1.Chat.Completions.Stream qualified as CCS
import OpenAI.V1.ToolCall qualified as OToolCall
import OpenAI.V1.Usage qualified as OUsage

chunkToStreamEvents :: CCS.ChatCompletionChunk -> [StreamEvent]
chunkToStreamEvents chunk =
  case Vector.toList (CCS.choices chunk) of
    [] -> maybe [] (\u -> [fromUsage u]) (CCS.usage chunk)
    cs -> concatMap choiceEvents cs
  where
    choiceEvents c = textEvents (CCS.delta c) ++ toolCallEvents (CCS.delta c) ++ finishEvent c

    textEvents delta = case CCS.delta_content delta of
      Just t | not (Text.null t) -> [StreamTextDelta t]
      _ -> []

    toolCallEvents delta = case CCS.delta_tool_calls delta of
      Just tcs -> Vector.toList (Vector.imap toolCallToEvent tcs)
      Nothing -> []

    toolCallToEvent _ tc =
      let tcId = OToolCall.id (tc :: OToolCall.ToolCall)
          tcName = OToolCall.name (OToolCall.function tc)
       in StreamToolCallStart tcId tcName

    finishEvent c = case CCS.finish_reason c of
      Just fr | fr == "stop" || fr == "length" || fr == "content_filter" || fr == "tool_calls" -> []
      _ -> []

    fromUsage u =
      StreamUsage
        (fromIntegral (OUsage.prompt_tokens u))
        (fromIntegral (OUsage.completion_tokens u))
        (fromIntegral (OUsage.total_tokens u))
