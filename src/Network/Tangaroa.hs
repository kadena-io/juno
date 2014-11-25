{-# LANGUAGE RecordWildCards #-}

module Network.Tangaroa
  ( module Network.Tangaroa.Types
  , raft
  , RaftSpec(..)
  ) where

import Network.Tangaroa.Types

import Control.Lens
import Control.Monad

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan.Unagi

-- A structure containing all the implementation details for running
-- the raft protocol.
data RaftSpec nt et rt mt ht = RaftSpec
  {
    -- ^ Function to commit a log entry.
    commit          :: et -> IO rt

    -- ^ Function to obtain persistent state.
  , getPS           :: IO (PersistentState nt et)

    -- ^ Function to obtain configuration.
  , getCfg          :: IO (Config nt et)

  -- ^ Function to write persistent state.
  , writePS         :: PersistentState nt et -> IO ()

  -- ^ Function to open a connection handle.
  , openConnection  :: nt -> IO ht

  -- ^ Function to serialize a result.
  , serializeResult :: rt -> mt

  -- ^ Function to serialize a Raft RPC.
  , serializeRPC    :: RPC nt et -> mt

  -- ^ Function to serialize a Raft RPC.
  , deserializeRPC  :: mt -> RPC nt et

  -- ^ Function to send a message to a node.
  , sendMessage     :: nt -> mt -> IO ()

  -- ^ Function to get the next message.
  , getMessage      :: ht -> IO mt
  }

data Event mt = Message mt
              | Election
              | Heartbeat

raft :: RaftSpec nt et rt mt ht -> IO ()
raft rs@RaftSpec{..} = do
  ps <- getPS
  cfg <- getCfg
  h <- openConnection (cfg ^. cfgNodeId)
  (eventIn, eventOut) <- newChan
  receiver <- forkIO (messageReceiver getMessage h eventIn)
  electionTimer <- forkIO $ timer Election eventIn $ cfg ^. cfgElectionTimeout
  heartbeatTimer <- forkIO $ timer Heartbeat eventIn $ cfg ^. cfgHeartbeatTimeout
  handleEvents rs eventOut

messageReceiver :: (ht -> IO mt) -> ht -> InChan (Event mt) -> IO ()
messageReceiver getMsg h chan =
  forever $ getMsg h >>= writeChan chan . Message

timer :: Event mt -> InChan (Event mt) -> Int -> IO ()
timer e chan timeout = forever $ do
  threadDelay timeout
  writeChan chan e

handleEvents :: RaftSpec nt et rt mt ht -> OutChan (Event mt) -> IO ()
handleEvents RaftSpec{..} chan = forever $ do
  e <- readChan chan
  case e of
    Message _ -> return ()
    Election -> putStrLn "Got election timeout."
    Heartbeat -> putStrLn "Got heartbeat timeout."
