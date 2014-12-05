{-# LANGUAGE FlexibleContexts #-}

module Network.Tangaroa.Util
  ( seqIndex
  , lastLogInfo
  , getQuorumSize
  , debug
  , fork_
  , runRWS_
  , enqueueEvent
  , dequeueEvent
  , messageReceiver
  ) where

import Network.Tangaroa.Types
import Control.Lens
import Data.Sequence (Seq)
import Control.Monad.RWS
import Control.Concurrent.Lifted (fork)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Concurrent.Chan.Unagi
import qualified Data.Sequence as Seq

seqIndex :: Seq a -> Int -> Maybe a
seqIndex s i =
  if i >= 0 && i < Seq.length s
    then Just (Seq.index s i)
    else Nothing

getQuorumSize :: Int -> Int
getQuorumSize n =
  if even n
    then n `div` 2 + 1
    else (n - 1) `div` 2 + 1

-- get the last term and index of a log
lastLogInfo :: Seq (Term, et) -> (Term, LogIndex)
lastLogInfo es =
  case Seq.viewr es of
    _ Seq.:> (t,_) -> (t, Seq.length es - 1)
    Seq.EmptyR     -> (startTerm, startIndex)

debug :: String -> Raft nt et rt mt ()
debug s = do
  dbg <- view (rs.debugPrint)
  nid <- view (cfg.nodeId)
  dbg nid s

fork_ :: MonadBaseControl IO m => m () -> m ()
fork_ a = fork a >> return ()

runRWS_ :: Monad m => RWST r w s m a -> r -> s -> m ()
runRWS_ ma r s = runRWST ma r s >> return ()

enqueueEvent :: Event nt et rt -> Raft nt et rt mt ()
enqueueEvent event = do
  ein <- view eventIn
  lift $ writeChan ein event

dequeueEvent :: Raft nt et rt mt (Event nt et rt)
dequeueEvent = lift . readChan =<< view eventOut

-- | Thread to take incoming messages and write them to the event queue.
messageReceiver :: Raft nt et rt mt ()
messageReceiver = do
  gm <- view (rs.getMessage)
  deser <- view (rs.deserializeRPC)
  forever $
    gm >>= maybe
      (debug "failed to deserialize RPC")
      (enqueueEvent . ERPC)
      . deser
