# Agents.hs

> **Haskell LLM Provider Abstraction** — type-safe, effectful, streaming.  
> Inspired by the [Vercel AI SDK](https://sdk.vercel.ai/) for TypeScript.



**Agents.hs** is a multi-package Haskell library that provides a provider-agnostic abstraction layer for interacting with Large Language Models. Swap between OpenAI and Anthropic Claude with zero code changes. Built on [Effectful](https://hackage.haskell.org/package/effectful) for effects and [Conduit](https://hackage.haskell.org/package/conduit) for streaming.

> **This library was developed with heavy use of AI agents** — from architecture design to implementation, testing, and documentation. AI-assisted development helped iterate rapidly across multiple providers, effect systems, and streaming pipelines.

---

## Features

### Implemented

| Feature | Description |
|---------|-------------|
| **Multi-provider abstraction** | Swap between OpenAI and Claude via the `Provider` typeclass |
| **Non-streaming generation** | `generate` on both OpenAI (Chat Completions) and Claude (Messages API) |
| **Rich streaming** | Conduit-based `stream` emitting text deltas, tool calls, thinking, and usage events |
| **Full tool calling lifecycle** | `respond` with `ToolDefinition` → `ToolCall` → `ToolResult` flow |
| **Mixed content messages** | Messages support text, base64 images, tool calls, tool results, and escape hatch |
| **Automatic JSON Schema derivation** | `HasJsonSchema` with GHC.Generics — derive JSON Schema from Haskell record types |
| **Typed tool handlers** | Existential `ToolHandler` with `FromJSON` constraint — auto-parse arguments |
| **Agent loop** | `runAgent` drives the LLM ⇄ tool loop with max steps and full execution traces |
| **Streaming agent loop** | `streamAgent` combines real-time streaming with automatic tool calling |
| **Tool execution error handling** | Three failure modes caught and fed back to the LLM for self-correction |
| **Pluggable memory** | `MemoryProvider` record-of-functions — swap in-memory storage for persistent backends |
| **Separation of config** | `ProviderConfig` (connection) vs `GenerationConfig` (per-call parameters) |
| **Rich error types** | 10 `ProviderError` + 2 `AgentError` constructors via Effectful's `Error` effect |
| **Streaming tool call assembly** | Full `Start → Delta → End` lifecycle for tool calls in both OpenAI and Claude streams |
| **Claude extended thinking** | `StreamThinkingDelta` support for Claude's reasoning/thinking feature |
| **Image content** | `ImageContent` with base64 data — converted to OpenAI data URIs and Claude image blocks |
| **Structured output** | `gcResponseSchema` mapped to OpenAI `response_format` and Claude `output_config` |
| **Stop sequences** | `gcStopSequences` mapped to both providers |
| **Reproducible dev environment** | Nix/devenv shell with GHC 9.14.1 |

### Yet to be Implemented

| Feature | Notes |
|---------|-------|
| **OpenAI Responsess API for `respond`** | Currently delegates to `generate`; should use the dedicated Responses API |
| **Unit test suite** | Only manual integration tests exist (`cabal run`); no `cabal test` target |
| **Retry with backoff** | `RateLimitError` and transient failures are not retried |
| **Configurable timeouts** | HTTP timeouts not exposed in `ProviderConfig` |
| **Observability / logging** | No built-in request/response logging |
| **Parallel tool execution** | Agent loop executes tools sequentially; OpenAI supports `parallel_tool_calls` |
| **URL-based images** | `ImageContent` only supports base64; URL-based images not yet modeled |
| **Tool choice control** | Cannot force a specific tool call (`tool_choice` not exposed) |
| **Token budget enforcement** | Usage is tracked but not used for budget limiting |
| **Claude cache token reporting** | Claude's cache hit/miss tokens not captured in `UsageInfo` |
| **Extended thinking toggle** | Claude thinking feature not configurable via `GenerationConfig` |

---

## Architecture

```
┌─────────────────────────────────────────────────┐
│                  Your Application                │
├─────────────────────────────────────────────────┤
│                                                 │
│  ┌──────────┐  ┌───────────────┐  ┌──────────┐ │
│  │  Agent   │  │  Streaming    │  │  Memory  │ │
│  │  Loop    │  │  Agent Loop   │  │ Provider │ │
│  └────┬─────┘  └───────┬───────┘  └──────────┘ │
│       │                │                         │
│  ┌────▼────────────────▼──────────────────────┐ │
│  │           Provider Typeclass               │ │
│  │  generate │ stream │ respond               │ │
│  └────┬───────────────────┬───────────────────┘ │
│       │                   │                     │
│  ┌────▼─────┐       ┌─────▼──────┐             │
│  │  OpenAI  │       │   Claude   │             │
│  │ Provider │       │  Provider  │             │
│  └──────────┘       └────────────┘             │
│                                                 │
│  ┌──────────────────────────────────────────┐  │
│  │  Types • Schema • Tools • Stream Events  │  │
│  └──────────────────────────────────────────┘  │
│               agents-core                       │
└─────────────────────────────────────────────────┘
```

### Core Design Decisions

- **Separate operations** — `generate`, `stream`, `respond` are distinct methods, not a unified `complete`. Clear intent at call sites; provider-specific optimizations per operation.
- **Escape hatch** — `EscapeHatchContent Value` lets you pass through provider-specific JSON not yet modeled in the agnostic types (e.g., OpenAI audio, Claude cache hints).
- **Effectful over mtl** — Uses the Effectful library for algebraic effects. All provider methods require `IOE :> es` and `Error ProviderError :> es`.
- **Conduit streaming** — Streaming uses Conduit for proper resource management. The `forkIO` + `TChan` pattern bridges callback-based SDKs into a clean conduit source.
- **Typed tools via existentials** — `ToolHandler` hides the concrete argument type behind an existential, allowing heterogeneous tool registries while maintaining type safety.

---

## Packages

| Package | Description |
|---------|-------------|
| [`agents-core`](packages/agents-core/) | Provider typeclass, message types, tool system, streaming events, schema derivation, agent loop, memory |
| [`agents-openai`](packages/agents-openai/) | OpenAI Chat Completions provider — wraps the [`openai`](https://hackage.haskell.org/package/openai) package |
| [`agents-claude`](packages/agents-claude/) | Anthropic Claude provider — wraps the [`claude`](https://hackage.haskell.org/package/claude) package |
| `agents-openai-test` | Manual integration test executable for OpenAI (hits real API) |
| `agents-claude-test` | Manual integration test executable for Claude (hits real API) |
| `agents-agent-test` | Manual integration test for the agent loop with tool calling |

---

## Quick Start

### Prerequisites

- [GHC 9.14.1](https://www.haskell.org/ghc/) and [Cabal](https://www.haskell.org/cabal/)
- Or: [Nix](https://nixos.org/) with [devenv](https://devenv.sh/) (`devenv shell` to enter the dev environment)

### Setup

```bash
git clone https://github.com/karmakarmeghdip/agents-hs.git
cd agents-hs

# Create .env with your API keys
cp .env.example .env
# Edit .env: add OPENAI_API_KEY and/or ANTHROPIC_API_KEY

# Build all packages
cabal build
```

### Run Integration Tests

```bash
cabal run agents-openai-test    # OpenAI: generate, stream, tool calling
cabal run agents-claude-test     # Claude: generate, stream, tool calling
cabal run agents-agent-test      # Agent loop with tool calling (uses OpenAI)
```

---

## Example

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import Agents.Provider
import Agents.Provider.OpenAI
import Agents.Types
import Agents.Tool
import Agents.Agent
import Agents.Memory
import Agents.Schema
import Effectful
import Effectful.Error.Static
import Data.Aeson
import GHC.Generics
import qualified Data.Text as T

-- Define a tool with auto-derived JSON Schema
data WeatherArgs = WeatherArgs { location :: T.Text }
  deriving (Show, Generic, HasJsonSchema)
  deriving anyclass (FromJSON)

-- Create an agent
main :: IO ()
main = do
  let providerCfg = ProviderConfig
        { pcApiKey = "sk-..."
        , pcBaseUrl = "https://api.openai.com"
        }
  let genCfg = GenerationConfig
        { gcModel = "gpt-4o"
        , gcMaxTokens = Just 1024
        , gcTemperature = Just 0.7
        , gcTopP = Nothing
        , gcStopSequences = []
        , gcResponseSchema = Nothing
        }

  provider <- newOpenAIProvider providerCfg
  memory <- newInMemoryMemory 256
  let weatherTool = tool "get_weather" "Get current weather for a city" $ \args ->
        pure $ toJSON $ object ["temperature" .= (22 :: Int), "condition" .= ("sunny" :: T.Text)]

  let agent = Agent
        { agentProvider = provider
        , agentProviderCfg = providerCfg
        , agentGenerationCfg = genCfg
        , agentTools = registerTool weatherTool emptyToolRegistry
        , agentMemory = memory
        , agentMaxSteps = 5
        , agentSystemPrompt = Just "You are a helpful weather assistant."
        }

  result <- runEff $
    runErrorNoCallStack @AgentError $
    runAgent agent (mkUserMessage "What's the weather in London?")

  case result of
    Left err -> print err
    Right r  -> T.putStrLn $ arText r
```

---

## Streaming Event Types

The `stream` method yields these events through a Conduit:

| Event | Description |
|-------|-------------|
| `StreamTextDelta` | Text token produced by the model |
| `StreamToolCallStart` | A tool call has begun (id + name) |
| `StreamToolCallDelta` | Partial tool call arguments (accumulated) |
| `StreamToolCallEnd` | Tool call complete with full arguments |
| `StreamThinkingDelta` | Claude extended thinking/reasoning content |
| `StreamUsage` | Token usage statistics at stream end |
| `StreamError` | In-stream error message |
| `StreamDone` | Stream completed successfully |

---

## License

BSD-3-Clause © 2026 karmakarmeghdip. See [LICENSE](LICENSE).

---

*Built with Haskell, Effectful, Conduit — and a lot of help from AI agents.*
