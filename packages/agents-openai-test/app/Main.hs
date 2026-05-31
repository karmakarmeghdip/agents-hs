{-# LANGUAGE OverloadedStrings #-}

module Main where

import Agents.Error (ProviderError (..))
import Agents.Provider (Provider (..))
import Agents.Provider.OpenAI (newOpenAIProvider)
import Agents.StreamEvent (StreamEvent (..))
import Agents.Tool (ToolDefinition (..))
import Agents.Types
  ( CompletionResponse (..),
    ContentBlock (..),
    GenerationConfig (..),
    Message (..),
    MessageRole (..),
    ProviderConfig (..),
  )
import Control.Exception (SomeException, try)
import Data.Aeson qualified as Aeson
import Data.Conduit (runConduit, (.|))
import Data.Conduit.List qualified as CL
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Data.Vector qualified as Vector
import Effectful (runEff)
import Effectful qualified as E
import Effectful.Error.Static (runError)
import System.Directory (doesFileExist)
import System.Environment (getEnv, setEnv)

main :: IO ()
main = do
  putStrLn "=== Loading configuration ==="
  loadDotEnv
  apiKey <- getEnv "OPENAI_API_KEY"
  let config =
        ProviderConfig
          { pcApiKey = Text.pack (stripQuotes apiKey),
            pcBaseUrl = "https://api.openai.com"
          }

  putStrLn "=== Creating OpenAI provider ==="
  provider <- newOpenAIProvider config
  putStrLn "Provider created successfully!"

  let genConfig =
        GenerationConfig
          { gcModel = "gpt-5.4-mini",
            gcMaxTokens = Just 100,
            gcTemperature = Just 0.7,
            gcTopP = Nothing,
            gcStopSequences = Nothing,
            gcResponseSchema = Nothing
          }

  -- Test 1: Simple text generation
  putStrLn "\n=== Test 1: Simple text generation ==="
  let messages1 =
        [ Message System [TextContent "You are a helpful assistant. Be brief."],
          Message User [TextContent "What is 2 + 2? Answer in one word."]
        ]

  result1 <- runEff $ runError @ProviderError $ generate provider config genConfig messages1 []
  case result1 of
    Left (_, err) -> putStrLn $ "ERROR: " ++ show err
    Right resp -> do
      putStrLn $ "Model: " ++ show (crModel resp)
      putStrLn $ "Finish reason: " ++ show (crFinishReason resp)
      putStrLn $ "Usage: " ++ show (crUsage resp)
      putStrLn $ "Content: " ++ show (crContent resp)

  -- Test 2: Streaming text generation
  putStrLn "\n=== Test 2: Streaming text generation ==="
  let messages2 =
        [ Message User [TextContent "Say hello in 3 languages, briefly."]
        ]

  result2 <- runEff $ runError @ProviderError $ do
    src <- stream provider config genConfig messages2 []
    runConduit $ src .| CL.mapM_ (\ev -> E.liftIO $ printStreamEvent ev)
  case result2 of
    Left err -> putStrLn $ "ERROR: " ++ show err
    Right _ -> putStrLn "(stream finished)"

  -- Test 3: Tool calling (respond)
  putStrLn "\n=== Test 3: Tool calling ==="
  let weatherTool =
        ToolDefinition
          { tdName = "get_weather",
            tdDescription = "Get the current weather for a city",
            tdParameters =
              Aeson.object
                [ "type" Aeson..= ("object" :: Text),
                  "properties"
                    Aeson..= Aeson.object
                      [ "city"
                          Aeson..= Aeson.object
                            [ "type" Aeson..= ("string" :: Text),
                              "description" Aeson..= ("City name" :: Text)
                            ]
                      ],
                  "required" Aeson..= Aeson.Array (Vector.fromList [Aeson.String "city"])
                ]
          }
  let messages3 =
        [ Message User [TextContent "What's the weather in Tokyo?"]
        ]

  result3 <- runEff $ runError @ProviderError $ respond provider config genConfig messages3 [weatherTool]
  case result3 of
    Left (_, err) -> putStrLn $ "ERROR: " ++ show err
    Right resp -> do
      putStrLn $ "Model: " ++ show (crModel resp)
      putStrLn $ "Finish reason: " ++ show (crFinishReason resp)
      putStrLn $ "Content: " ++ show (crContent resp)

printStreamEvent :: StreamEvent -> IO ()
printStreamEvent (StreamTextDelta t) = TIO.putStr t
printStreamEvent (StreamToolCallStart i n) = putStrLn $ "\n[ToolCall Start: " ++ show n ++ " id=" ++ show i ++ "]"
printStreamEvent (StreamToolCallDelta i v) = putStrLn $ "\n[ToolCall Delta: id=" ++ show i ++ " args=" ++ show v ++ "]"
printStreamEvent (StreamToolCallEnd i n v) = putStrLn $ "\n[ToolCall End: " ++ show n ++ " id=" ++ show i ++ " args=" ++ show v ++ "]"
printStreamEvent (StreamThinkingDelta t) = TIO.putStr $ "[Thinking: " <> t <> "]"
printStreamEvent (StreamUsage p c t) = putStrLn $ "\n[Usage: prompt=" ++ show p ++ " completion=" ++ show c ++ " total=" ++ show t ++ "]"
printStreamEvent (StreamError err) = putStrLn $ "\n[Stream Error: " ++ show err ++ "]"
printStreamEvent StreamDone = putStrLn "\n[Stream Done]"

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
      _ <- try @(SomeException) (setEnv key value)
      pure ()
