{-# LANGUAGE RecordWildCards #-}

module Network.Tangaroa
  ( runRaft
  , module Network.Tangaroa.Types
  ) where

import Network.Tangaroa.Types
import Network.Tangaroa.Internal.Monad
import Network.Tangaroa.Internal.State
import System.Random

import Control.Lens hiding (Index)
import Control.Monad
import Control.Monad.Trans

import Control.Monad.RWS

import Control.Concurrent.Chan.Unagi

runRaft :: RaftSpec nt et rt mt ht -> IO ()
runRaft spec@RaftSpec{..} = do
  rconf <- _readCfg
  (ein, eout) <- newChan
  h <- _openConnection (rconf ^. nodeId)
  runRWS_ raft (RaftEnv rconf h ein eout spec) initialRaftState

runRWS_ :: Monad m => RWST r w s m a -> r -> s -> m ()
runRWS_ ma r s = runRWST ma r s >> return ()

raft :: Raft nt et rt mt ht ()
raft = do
  _ <- fork $ electionLoop
  _ <- fork $ messageReceiver
  handleEvents

-- | Thread to take incoming messages and write them to the event queue.
messageReceiver :: Raft nt et rt mt ht ()
messageReceiver = do
  h <- view conn
  gm <- view (rs.getMessage)
  forever $ do
    m <- lift $ gm h
    sendEvent (Message m)

-- | Thread to generate random timeout events within a range.
electionLoop :: Raft nt et rt mt ht ()
electionLoop = forever $ do
  timeout <- lift . randomRIO =<< view (cfg.electionTimeoutRange)
  wait timeout
  sendEvent $ Election $ show (timeout `div` 1000) ++ "ms"

getEvent :: Raft nt et rt mt ht (Event mt)
getEvent = lift . readChan =<< view eventOut

handleEvents :: Raft nt et rt mt ht ()
handleEvents = forever $ do
  e <- getEvent
  case e of
    Message m   -> handleMessage m
    Election s  -> handleElectionTimeout s
    Heartbeat s -> handleHeartbeatTimeout s

handleElectionTimeout :: String -> Raft nt et rt mt ht ()
handleElectionTimeout s = lift $ putStr "Election timeout: " >> putStrLn s
-- TODO

handleHeartbeatTimeout :: String -> Raft nt et rt mt ht ()
handleHeartbeatTimeout s = lift $ putStr "Heartbeat timeout: " >> putStrLn s
-- TODO

handleMessage :: mt -> Raft nt et rt mt ht ()
handleMessage m = do
  dm <- rs.deserializeRPC ^$ m
  case dm of
    Just (AE ae)     -> handleAppendEntries ae
    Just (AER aer)   -> handleAppendEntriesResponse aer
    Just (RV rv)     -> handleRequestVote rv
    Just (RVR rvr)   -> handleRequestVoteResponse rvr
    Just (CMD cmd)   -> handleCommand cmd
    Just (CMDR _)    -> lift $ putStrLn "Got a command response RPC."
    Just (DBG s)     -> lift $ putStrLn $ "Debug RPC: " ++ s
    Nothing          -> lift $ putStrLn "RPC failed to deserialize."
