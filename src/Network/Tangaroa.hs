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

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan.Unagi

runRaft :: RaftSpec nt et rt mt ht -> IO ()
runRaft rs@RaftSpec{..} = do
  rconf <- readCfg
  (ein, eout) <- newChan
  h <- openConnection (rconf ^. cfgNodeId)
  receiver <- forkIO (messageReceiver getMessage h ein)
  runRWS_ (raft rs) (RaftEnv rconf ein eout) initialVolatileState

runRWS_ :: Monad m => RWST r w s m a -> r -> s -> m ()
runRWS_ ma r s = runRWST ma r s >> return ()

raft :: RaftSpec nt et rt mt ht -> Raft nt mt
raft rs@RaftSpec{..} = do
  ein <- view eventIn
  eout <- view eventOut
  electionTimer <- lift . forkIO . electionLoop ein =<< view (cfg.cfgElectionTimeoutRange)
  handleEvents rs eout

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

handleEvents :: RaftSpec nt et rt mt ht -> OutChan (Event mt) -> Raft nt mt
handleEvents RaftSpec{..} chan = forever $ do
  e <- lift (readChan chan)
  lift $ case e of
    Message _   -> return ()
    Election s  -> putStr "Got election timeout: " >> putStrLn s
    Heartbeat s -> putStr "Got heartbeat timeout: " >> putStrLn s
