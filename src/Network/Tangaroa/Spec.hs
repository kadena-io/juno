module Network.Tangaroa.Spec
  ( RaftSpec(..)
  ) where

import Network.Tangaroa.Types

-- | A structure containing all the implementation details for running
-- the raft protocol.
data RaftSpec nt et rt mt ht = RaftSpec
  {
    -- ^ Function to read configuration.
    readCfg          :: IO (Config nt)

    -- ^ Function to get a log entry from persistent storage.
  , readLogEntry     :: Index -> IO et

    -- ^ Function to write a log entry to persistent storage.
  , writeLogEntry    :: Index -> et -> IO ()

    -- ^ Function to get the term number from persistent storage.
  , readTermNumber   :: IO Term

    -- ^ Function to write the term number to persistent storage.
  , writeTermNumber   :: Term -> IO ()

    -- ^ Function to read the node voted for from persistent storage.
  , readVotedFor      :: IO (Maybe nt)

    -- ^ Function to write the node voted for to persistent storage.
  , writeVotedFor     :: nt -> IO ()

    -- ^ Function to commit a log entry.
  , commit           :: et -> IO rt

    -- ^ Function to open a connection handle.
  , openConnection   :: nt -> IO ht

    -- ^ Function to serialize an RPC.
  , serializeRPC     :: RPC nt et rt -> mt

    -- ^ Function to deserialize an RPC.
  , deserializeRPC   :: mt -> RPC nt et rt

    -- ^ Function to send a message to a node.
  , sendMessage      :: nt -> mt -> IO ()

    -- ^ Function to get the next message.
  , getMessage       :: ht -> IO mt
  }
