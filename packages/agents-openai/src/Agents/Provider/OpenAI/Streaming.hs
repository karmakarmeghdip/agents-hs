{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Agents.Provider.OpenAI.Streaming
  ( ToolCallAccum(..)
  , AccumState(..)
  , emptyAccumState
  , processChunk
  , finalizeAccumState
  ) where

import Agents.StreamEvent (StreamEvent (..))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as Vector
import OpenAI.V1.Chat.Completions.Stream qualified as CCS
import OpenAI.V1.ToolCall qualified as OToolCall
import OpenAI.V1.Usage qualified as OUsage

data ToolCallAccum = ToolCallAccum
    { tcaId        :: !Text
    , tcaName      :: !Text
    , tcaArguments :: !Text
    }

data AccumState = AccumState
    { asToolCalls :: !(IntMap ToolCallAccum)
    }

emptyAccumState :: AccumState
emptyAccumState = AccumState { asToolCalls = IntMap.empty }

parsePartialArgs :: Text -> Aeson.Value
parsePartialArgs txt
    | Text.null txt = Aeson.object []
    | otherwise     = case Aeson.decode (LBS.fromStrict (TE.encodeUtf8 txt)) of
        Just v  -> v
        Nothing -> Aeson.toJSON txt

processChunk :: AccumState -> CCS.ChatCompletionChunk -> (AccumState, [StreamEvent])
processChunk accumState chunk =
  case Vector.toList (CCS.choices chunk) of
    [] -> (accumState, maybe [] (\u -> [fromUsage u]) (CCS.usage chunk))
    cs -> let (finalAccum, allEvents) = foldl goChoice (accumState, []) cs
          in (finalAccum, concat (reverse allEvents))
  where
    goChoice (acc, evts) c =
      let (acc', choiceEvts) = processChoice acc c
      in (acc', choiceEvts : evts)

    processChoice acc c =
        let (acc1, textEvts) = processText acc (CCS.delta c)
            (acc2, tcEvts) = processToolCalls acc1 (CCS.delta c)
            (acc3, finishEvts) = processFinish acc2 c
        in (acc3, textEvts ++ tcEvts ++ finishEvts)

    processText acc delta = case CCS.delta_content delta of
        Just t | not (Text.null t) -> (acc, [StreamTextDelta t])
        _ -> (acc, [])

    processToolCalls acc delta = case CCS.delta_tool_calls delta of
        Just tcs -> Vector.ifoldl' (\(curAcc, evtsSoFar) idx tc -> processOneToolCall curAcc evtsSoFar idx tc) (acc, []) tcs
        Nothing -> (acc, [])

    processOneToolCall curAcc evts idx tc =
        let tcId = OToolCall.id (tc :: OToolCall.ToolCall)
            tcFunc = OToolCall.function tc
            tcName = OToolCall.name tcFunc
            tcArgsFragment = OToolCall.arguments tcFunc
            isContinuation = Text.null tcId
        in if isContinuation
           then case IntMap.lookup idx (asToolCalls curAcc) of
               Just existing ->
                   let newArgs = tcaArguments existing <> tcArgsFragment
                       newAccum = existing { tcaArguments = newArgs }
                       newAcc = curAcc { asToolCalls = IntMap.insert idx newAccum (asToolCalls curAcc) }
                   in if Text.null tcArgsFragment
                      then (newAcc, evts)
                      else (newAcc, evts ++ [StreamToolCallDelta (tcaId existing) (parsePartialArgs newArgs)])
               Nothing ->
                   (curAcc, evts)
           else case IntMap.lookup idx (asToolCalls curAcc) of
               Just existing ->
                   let newArgs = tcaArguments existing <> tcArgsFragment
                       newAcc = curAcc { asToolCalls = IntMap.insert idx existing { tcaArguments = newArgs } (asToolCalls curAcc) }
                   in (newAcc, evts ++ [StreamToolCallDelta tcId (parsePartialArgs newArgs)])
               Nothing ->
                   let newAccum = ToolCallAccum
                           { tcaId = tcId
                           , tcaName = tcName
                           , tcaArguments = tcArgsFragment
                           }
                       newAcc = curAcc { asToolCalls = IntMap.insert idx newAccum (asToolCalls curAcc) }
                   in if Text.null tcArgsFragment
                      then (newAcc, evts ++ [StreamToolCallStart tcId tcName])
                      else (newAcc, evts ++ [StreamToolCallStart tcId tcName, StreamToolCallDelta tcId (parsePartialArgs tcArgsFragment)])

    processFinish acc c = case CCS.finish_reason c of
        Just fr | fr == "tool_calls" ->
            let evts = IntMap.foldr finalizeAccum [] (asToolCalls acc)
                newAcc = acc { asToolCalls = IntMap.empty }
            in (newAcc, evts)
        _ -> (acc, [])

    finalizeAccum tc evts =
        let parsedArgs = parsePartialArgs (tcaArguments tc)
        in StreamToolCallEnd (tcaId tc) (tcaName tc) parsedArgs : evts

    fromUsage u =
      StreamUsage
        (fromIntegral (OUsage.prompt_tokens u))
        (fromIntegral (OUsage.completion_tokens u))
        (fromIntegral (OUsage.total_tokens u))

finalizeAccumState :: AccumState -> [StreamEvent]
finalizeAccumState accumState =
    IntMap.foldr finalizeAccum' [] (asToolCalls accumState)
  where
    finalizeAccum' tc evts =
        let parsedArgs = parsePartialArgs (tcaArguments tc)
        in StreamToolCallEnd (tcaId tc) (tcaName tc) parsedArgs : evts