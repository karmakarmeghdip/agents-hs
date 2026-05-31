# AGENTS.md

## Project Overview

A multi-package Haskell library for LLM provider abstraction (similar to Vercel AI SDK for TypeScript). Uses cabal, GHC 9.14.1, Effectful for effects, Conduit for streaming.

## Critical Rule

**Never assume or make decisions on your own.** Ask the user for every minor detail. Grill the user until the entire concept is clear and implementable before writing any code.

## Build & Run

```bash
cabal build                          # Build all packages
cabal build agents-openai-test       # Build OpenAI test executable only
cabal build agents-claude-test       # Build Claude test executable only
cabal run agents-openai-test         # Run OpenAI tests (requires .env with OPENAI_API_KEY)
cabal run agents-claude-test         # Run Claude tests (requires .env with ANTHROPIC_API_KEY)
```

There is no test suite runner (`cabal test`). Tests are manual executables that hit the real APIs. You need a `.env` file at the project root with `OPENAI_API_KEY` and/or `ANTHROPIC_API_KEY`.

Important: `cabal.project` has `allow-newer: *` — this is required for GHC 9.14.1 (base 4.22) compatibility with servant-client. Do not remove it.

## Package Structure

```
packages/agents-core/         # Provider typeclass, message types, stream events, tool defs, schema, errors
packages/agents-openai/       # OpenAI Chat Completions provider (functional)
packages/agents-claude/       # Claude provider (functional — wraps claude Hackage package)
packages/agents-openai-test/  # Test executable hitting real OpenAI API
packages/agents-claude-test/  # Test executable hitting real Claude API
```

All packages share version `0.1.0.0`. All modules use the `Agents.*` hierarchy.

## Architecture

- **Provider typeclass** (`Agents.Provider`): `generate`, `stream`, `respond` — separate ops, not a unified `complete`
- **Config split**: `ProviderConfig` (per-instance: apiKey, baseUrl) vs `GenerationConfig` (per-call: model, temp, etc.)
- **Message model**: `Message` contains `[ContentBlock]` — supports mixed content (text, images, tool calls, tool results, escape hatch)
- **Escape hatch**: `EscapeHatchContent Value` for provider-specific JSON not yet modeled explicitly
- **Streaming**: Conduit-based, yields rich `StreamEvent` variants (not just text deltas)
- **Errors**: Effectful's `Error ProviderError` effect, not ExceptT
- **Tool calls**: Simple `ToolCall` record (id, name, args) — no full lifecycle model

## Key Gotchas

- **Claude base URL**: Use `"https://api.anthropic.com"` (default). The `claude` Hackage package handles the `/v1` path internally.
- **Claude streaming**: Uses `forkIO` + `TChan` to bridge the `claude` package's callback-based streaming into Conduit (same pattern as OpenAI).
- **Claude `respond`**: Delegates to `generate` (Claude uses a single Messages API for both).
- **Claude system messages**: Extracted from the message list and sent as the top-level `system` parameter per Claude API requirements.
- **Claude `max_tokens`**: Required by Claude API; defaults to 4096 if not specified in `GenerationConfig`.
- **OpenAI base URL**: Use `"https://api.openai.com"` (no `/v1` suffix). The `openai` Hackage package appends `/v1` internally.
- **OpenAI streaming**: Uses `forkIO` + `TChan` to bridge the `openai` package's callback-based streaming into Conduit.
- **OpenAI `respond`**: Currently delegates to `generate` (Responses API integration deferred).
- **`jsonSchema`**: Stub — `error "not yet implemented"`. Needs GHC.Generics implementation.
- **Streaming tool calls**: Only `StreamToolCallStart` is emitted from OpenAI streaming. Delta/assembly of tool call arguments is not yet implemented.
- **`.env` parsing**: The test executable has a hand-rolled `.env` parser. Values with double quotes are stripped with `stripQuotes` which removes both leading and trailing quotes.
- **No unit tests**: Only the manual integration test executable. No `cabal test` target.

## Incomplete / TODO

- `Agents.Schema.jsonSchema`: Not implemented
- OpenAI streaming: tool call delta assembly (`StreamToolCallDelta`, `StreamToolCallEnd`)
- OpenAI `respond`: Should use the Responses API for tool calling
- Claude streaming: tool call delta assembly (`StreamToolCallDelta`, `StreamToolCallEnd`) — currently only `StreamToolCallStart` is emitted