{-# LANGUAGE RecordWildCards #-}

module Network.Tangaroa
  ( module Network.Tangaroa.Types
  , runRaft
  , RaftSpec(..)
  ) where

import Network.Tangaroa.Types
import Network.Tangaroa.Monad
import System.Random

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Lens
import Control.Monad
import Control.Monad.Trans

import Control.Concurrent.Lifted.Fork
import Control.Monad.RWS.Concurrent

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
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
  , getCfg          :: IO (Config nt)

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
              | Election String
              | Heartbeat String

runRWSC_ :: MonadIO m => RWSC r w s m a -> r -> TVar s -> TVar w -> m ()
runRWSC_ act r stv wtv = runRWSC act r stv wtv >>= (\_ -> return ())

initialVolatileState :: VolatileState nt
initialVolatileState = VolatileState Follower startIndex startIndex

initialCandidateState :: CandidateState nt
initialCandidateState = CandidateState Map.empty

initialLeaderState :: LeaderState nt
initialLeaderState = LeaderState Map.empty Map.empty

runRaft :: RaftSpec nt et rt mt ht -> IO ()
runRaft rs = do
  stv <- newTVarIO initialVolatileState
  wtv <- newTVarIO ()
  cfg <- getCfg rs
  runRWSC_ (raft rs) cfg stv wtv

raft :: RaftSpec nt et rt mt ht -> Raft nt
raft rs@RaftSpec{..} = do
  ps <- lift getPS
  cfg <- lift getCfg
  h <- lift $ openConnection (cfg ^. cfgNodeId)
  (eventIn, eventOut) <- lift newChan
  receiver <- lift $ forkIO (messageReceiver getMessage h eventIn)
  electionTimer <- lift $ forkIO $ electionLoop eventIn $ cfg ^. cfgElectionTimeoutRange
  handleEvents rs eventOut


-- | Thread to take incoming messages and write them to the event queue.
messageReceiver :: (ht -> IO mt) -> ht -> InChan (Event mt) -> IO ()
messageReceiver getMsg h chan =
  forever $ getMsg h >>= writeChan chan . Message

-- | Thread to generate random timeout events within a range.
electionLoop :: InChan (Event mt) -> (Int,Int) -> IO ()
electionLoop chan range = forever $ do
  timeout <- randomRIO range
  threadDelay timeout
  writeChan chan $ Election $ show (timeout `div` 1000) ++ "ms"

handleEvents :: RaftSpec nt et rt mt ht -> OutChan (Event mt) -> Raft nt
handleEvents RaftSpec{..} chan = forever $ do
  e <- lift (readChan chan)
  lift $ case e of
    Message _   -> return ()
    Election s  -> putStr "Got election timeout: " >> putStrLn s
    Heartbeat s -> putStr "Got heartbeat timeout: " >> putStrLn s
