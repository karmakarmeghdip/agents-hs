{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Data.Aeson qualified as Aeson
import Data.Aeson (FromJSON)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful (runEff)
import Effectful.Error.Static (runError)
import System.Directory (doesFileExist)
import System.Environment (getEnv, setEnv)
import Control.Exception (SomeException, try)
import GHC.Generics (Generic)

import Agents.Agent (Agent (..), AgentError (..), AgentResult (..), AgentStep (..), runAgent)
import Agents.Memory (MemoryProvider (..), newInMemoryMemory)
import Agents.Provider (tool, HasJsonSchema(..))
import Agents.Provider.OpenAI (newOpenAIProvider)
import Agents.Types
    ( CompletionResponse (..)
    , ContentBlock (..)
    , GenerationConfig (..)
    , Message (..)
    , MessageRole (..)
    , ProviderConfig (..)
    , ToolResult (..)
    , ToolCall (..)
    )

data GetWeatherArgs = GetWeatherArgs
    { city :: Text
    , unit :: Maybe Text
    } deriving (Generic, FromJSON, HasJsonSchema)

weatherHandler :: GetWeatherArgs -> IO Aeson.Value
weatherHandler GetWeatherArgs{..} =
    let unitLabel = maybe "celsius" id unit
    in pure $ Aeson.object
        [ "city"        Aeson..= city
        , "temperature" Aeson..= (22 :: Int)
        , "unit"        Aeson..= unitLabel
        , "conditions"  Aeson..= ("partly cloudy" :: Text)
        , "humidity"    Aeson..= (65 :: Int)
        ]

main :: IO ()
main = do
    putStrLn "=== Agent Test: Multi-step tool calling with OpenAI-compatible API ==="
    loadDotEnv
    apiKey <- getEnv "OPENAI_API_KEY"

    let config = ProviderConfig
            { pcApiKey  = Text.pack (stripQuotes apiKey)
            , pcBaseUrl  = "https://opencode.ai/zen/go"
            }

    putStrLn "=== Creating OpenAI provider ==="
    provider <- newOpenAIProvider config
    putStrLn "Provider created successfully!"

    mem <- newInMemoryMemory

    let agent = Agent
            { agentProvider      = provider
            , agentProviderCfg   = config
            , agentGenerationCfg = GenerationConfig
                { gcModel          = "kimi-k2.6"
                , gcMaxTokens      = Just 4096
                , gcTemperature    = Just 0.7
                , gcTopP           = Nothing
                , gcStopSequences   = Nothing
                , gcResponseSchema  = Nothing
                }
            , agentTools         = Map.fromList [("get_weather", tool "get_weather" "Get the current weather for a city. Returns temperature and conditions." weatherHandler)]
            , agentMemory        = mem
            , agentMaxSteps      = 10
            , agentSystemPrompt  = Just "You are a helpful weather assistant. When asked about weather, use the get_weather tool. Always respond concisely."
            }

    -- Test 1: Print the auto-derived schema to verify it works
    putStrLn "\n=== Auto-derived JSON Schema for GetWeatherArgs ==="
    putStrLn $ "Schema: " ++ show (Aeson.encode (Agents.Provider.jsonSchema @GetWeatherArgs))

    putStrLn "\n=== Test 1: Simple question (no tool call) ==="
    result1 <- runEff $ runError @AgentError $ runAgent agent (Message User [TextContent "What is the capital of France?"])
    case result1 of
        Left (_, err) -> putStrLn $ "ERROR: " ++ show err
        Right res -> do
            putStrLn $ "Final text: " ++ show (arText res)
            putStrLn $ "Finish reason: " ++ show (arFinishReason res)
            putStrLn $ "Steps taken: " ++ show (length (arSteps res))
            printSteps (arSteps res)

    putStrLn "\n=== Test 2: Tool call (weather query) ==="
    mpClear mem
    result2 <- runEff $ runError @AgentError $ runAgent agent (Message User [TextContent "What's the weather in Tokyo?"])
    case result2 of
        Left (_, err) -> putStrLn $ "ERROR: " ++ show err
        Right res -> do
            putStrLn $ "Final text: " ++ show (arText res)
            putStrLn $ "Finish reason: " ++ show (arFinishReason res)
            putStrLn $ "Steps taken: " ++ show (length (arSteps res))
            printSteps (arSteps res)

    putStrLn "\n=== Test 3: Multi-step tool calls ==="
    mpClear mem
    result3 <- runEff $ runError @AgentError $ runAgent agent (Message User [TextContent "Compare the weather in London and New York."])
    case result3 of
        Left (_, err) -> putStrLn $ "ERROR: " ++ show err
        Right res -> do
            putStrLn $ "Final text: " ++ show (arText res)
            putStrLn $ "Finish reason: " ++ show (arFinishReason res)
            putStrLn $ "Steps taken: " ++ show (length (arSteps res))
            printSteps (arSteps res)

printSteps :: [AgentStep] -> IO ()
printSteps = mapM_ printStep
  where
    printStep (StepLLM resp) = do
        putStrLn $ "  [LLM] finish=" ++ show (crFinishReason resp)
        putStrLn $ "  [LLM] content=" ++ show (crContent resp)
    printStep (StepTool tc tr) = do
        putStrLn $ "  [Tool] name=" ++ show (tcName tc)
        putStrLn $ "  [Tool] args=" ++ show (tcArguments tc)
        putStrLn $ "  [Tool] result=" ++ show (trResult tr)
        putStrLn $ "  [Tool] isError=" ++ show (trIsError tr)

stripQuotes :: String -> String
stripQuotes ('"' : xs) | not (null xs) && last xs == '"' = init xs
stripQuotes ('\'' : xs) | not (null xs) && last xs == '\'' = init xs
stripQuotes s = s

loadDotEnv :: IO ()
loadDotEnv = do
    exists <- doesFileExist ".env"
    if exists
        then do
            contents <- readFile ".env"
            mapM_ setLine (lines contents)
        else putStrLn "No .env file found, relying on environment variables"
  where
    setLine line = case break (== '=') line of
        (key, '=' : value) -> safeSetEnv (strip key) (stripQuotes (strip value))
        _ -> pure ()
      where
        strip = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ') . dropWhile (== '\r')

    safeSetEnv key value = do
        _ <- try @SomeException (setEnv key value)
        pure ()