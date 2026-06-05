module Agents.Memory
    ( MemoryProvider(..)
    , newInMemoryMemory
    ) where

import Data.IORef (newIORef, readIORef, modifyIORef', writeIORef)
import Agents.Types (Message)

-- | A record-of-functions interface for conversation memory providers.
--
-- Memory providers store the conversation history (list of messages)
-- that gets passed to the LLM on each agent loop iteration.
--
-- === Architecture Decision: Record-of-Functions
--
-- We use a record-of-functions rather than a typeclass for extensibility.
-- This approach:
--
-- * Allows runtime composition and swapping of memory providers
-- * Avoids orphan instance issues common with typeclasses
-- * Makes it easy for downstream users to define custom providers
--   without creating new types and instances
-- * Works naturally with IO-based operations
--
-- === Creating a MemoryProvider
--
-- Use 'newInMemoryMemory' for a simple in-memory provider, or create
-- your own by providing the three functions:
--
-- @
-- myProvider :: IO MemoryProvider
-- myProvider = do
--     -- set up your storage (DB connection, file handle, etc.)
--     pure MemoryProvider
--         { mpGet   = ... -- read messages from storage
--         , mpAdd   = ... -- append message to storage
--         , mpClear = ... -- clear storage
--         }
-- @
data MemoryProvider = MemoryProvider
    { mpGet   :: IO [Message]
    -- ^ Retrieve all messages in the conversation history.
    --   Messages should be in chronological order (oldest first).

    , mpAdd   :: Message -> IO ()
    -- ^ Append a message to the conversation history.
    --   The message is added to the end of the existing history.

    , mpClear :: IO ()
    -- ^ Clear all messages from the conversation history.
    --   After calling this, @mpGet@ should return an empty list.
    }

-- | Create a simple in-memory memory provider.
--
-- Uses an 'IORef' to store the message list. Suitable for
-- single-session agents and testing. For persistence across
-- sessions, implement a custom 'MemoryProvider' with disk
-- or database storage.
newInMemoryMemory :: IO MemoryProvider
newInMemoryMemory = do
    ref <- newIORef []
    pure MemoryProvider
        { mpGet   = readIORef ref
        , mpAdd   = \msg -> modifyIORef' ref (++ [msg])
        , mpClear = writeIORef ref []
        }