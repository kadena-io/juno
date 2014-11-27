{-# LANGUAGE RecordWildCards #-}

module Network.Tangaroa
  ( runRaft
  , module Network.Tangaroa.Spec
  , module Network.Tangaroa.Types
  ) where

import Network.Tangaroa.Spec
import Network.Tangaroa.Types
import Network.Tangaroa.Internal.Monad
import Network.Tangaroa.Internal.State
import System.Random

import Control.Lens hiding (Index)
import Control.Monad
import Control.Monad.Trans

import Control.Monad.RWS

import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan.Unagi

runRaft :: RaftSpec nt et rt mt ht -> IO ()
runRaft rs@RaftSpec{..} = do
  rconf <- readCfg
  (ein, eout) <- newChan
  h <- openConnection (rconf ^. nodeId)
  runRWS_ (raft rs) (RaftEnv rconf h ein eout) initialRaftState

runRWS_ :: Monad m => RWST r w s m a -> r -> s -> m ()
runRWS_ ma r s = runRWST ma r s >> return ()

raft :: RaftSpec nt et rt mt ht -> Raft nt mt ht ()
raft rs@RaftSpec{..} = do
  ein <- view eventIn
  eout <- view eventOut
  h <- view conn
  electionTimer <- fork . electionLoop ein =<< view (cfg.electionTimeoutRange)
  receiver <- fork (messageReceiver getMessage h ein)
  handleEvents rs eout

-- | Thread to take incoming messages and write them to the event queue.
messageReceiver :: (ht -> IO mt) -> ht -> InChan (Event mt) -> Raft nt mt ht ()
messageReceiver getMsg h chan =
  lift . forever $ getMsg h >>= writeChan chan . Message

-- | Thread to generate random timeout events within a range.
electionLoop :: InChan (Event mt) -> (Int,Int) -> Raft nt mt ht ()
electionLoop chan range = lift . forever $ do
  timeout <- randomRIO range
  threadDelay timeout
  writeChan chan $ Election $ show (timeout `div` 1000) ++ "ms"

handleEvents :: RaftSpec nt et rt mt ht -> OutChan (Event mt) -> Raft nt mt ht ()
handleEvents rs@RaftSpec{..} chan = forever $ do
  e <- lift (readChan chan)
  case e of
    Message m   -> handleMessage rs m
    Election s  -> handleElectionTimeout rs s
    Heartbeat s -> handleHeartbeatTimeout rs s

handleElectionTimeout :: RaftSpec nt et rt mt ht -> String -> Raft nt mt ht ()
handleElectionTimeout _ s = lift $ putStr "Got election timeout: " >> putStrLn s
-- TODO

handleHeartbeatTimeout :: RaftSpec nt et rt mt ht -> String -> Raft nt mt ht ()
handleHeartbeatTimeout _ s = lift $ putStr "Got heartbeat timeout: " >> putStrLn s
-- TODO

handleMessage :: RaftSpec nt et rt mt ht -> mt -> Raft nt mt ht ()
handleMessage RaftSpec{..} m = case deserializeRPC m of
  Just (AE _)   -> lift $ putStrLn "Got an appendEntries RPC."
  Just (AER _)  -> lift $ putStrLn "Got an appendEntriesResponse RPC."
  Just (RV _)   -> lift $ putStrLn "Got a requestVote RPC."
  Just (RVR _)  -> lift $ putStrLn "Got a requestVoteResponse RPC."
  Just (CMD _)  -> lift $ putStrLn "Got a command RPC."
  Just (CMDR _) -> lift $ putStrLn "Got a command response RPC."
  Just (DBG s)  -> lift $ putStrLn $ "Got a debug RPC: " ++ s
  Nothing       -> lift $ putStrLn "Got a message, but it failed to deserialize."
-- TODO
