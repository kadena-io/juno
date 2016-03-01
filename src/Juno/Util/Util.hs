{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Juno.Util.Util
  ( seqIndex
  , lastLogInfo
  , getQuorumSize
  , debug
  , randomRIO
  , runRWS_
  , enqueueEvent, enqueueEventLater
  , dequeueEvent
  , messageReceiver
  , verifyRPCWithKey
  , verifyRPCWithClientKey
  , signRPCWithKey
  , updateTerm
  ) where

import Juno.Runtime.Types
import Juno.Util.Combinator

import Control.Lens
import Codec.Crypto.RSA
import Data.Sequence (Seq)
import Control.Monad.RWS
import qualified Control.Concurrent.Lifted as CL
import qualified Data.ByteString as B
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import qualified System.Random as R
import Data.Serialize

seqIndex :: Seq a -> Int -> Maybe a
seqIndex s i =
  if i >= 0 && i < Seq.length s
    then Just (Seq.index s i)
    else Nothing

getQuorumSize :: Int -> Int
getQuorumSize n = minimum [n - f | f <- [0..n], n >= 3*f + 1]

-- get the last term and index of a log
lastLogInfo :: Seq LogEntry -> (Term, LogIndex, B.ByteString)
lastLogInfo es =
  case Seq.viewr es of                 -- \/ TODO: This smells weird, should we really use length for this?
    _ Seq.:> LogEntry{..} -> (_leTerm, LogIndex $ Seq.length es - 1, _leHash)
    Seq.EmptyR            -> (startTerm, startIndex, B.empty)

debug :: Monad m => String -> Raft m ()
debug s = do
  dbg <- view (rs.debugPrint)
  nid <- view (cfg.nodeId)
  role' <- use role
  let prettyRole = case role' of
        Leader -> "\ESC[0;34m[LEADER]\ESC[0m"
        Follower -> "\ESC[0;32m[FOLLOWER]\ESC[0m"
        Candidate -> "\ESC[1;33m[CANDIDATE]\ESC[0m"
  dbg nid $ prettyRole ++ ": " ++ s

randomRIO :: (Monad m, R.Random a) => (a,a) -> Raft m a
randomRIO rng = view (rs.random) >>= \f -> f rng -- R.randomRIO

runRWS_ :: Monad m => RWST r w s m a -> r -> s -> m ()
runRWS_ ma r s = runRWST ma r s >> return ()

-- no state update
enqueueEvent :: Monad m => Event -> Raft m ()
enqueueEvent event = view (rs.enqueue) >>= \f -> f event
  -- lift $ writeChan ein event

enqueueEventLater :: Monad m => Int -> Event -> Raft m CL.ThreadId
enqueueEventLater t event = view (rs.enqueueLater) >>= \f -> f t event

-- no state update
dequeueEvent :: Monad m => Raft m Event
dequeueEvent = join $ view (rs.dequeue)

-- | Thread to take incoming messages and write them to the event queue.
-- THREAD: MESSAGE RECEIVER (client and server), no state updates
messageReceiver :: Monad m => Raft m ()
messageReceiver = do
  gm <- view (rs.getMessage)
  deser <- view (rs.deserializeRPC)
  forever $
    gm >>= either
      (debug . ("failed to deserialize RPC: " ++))
      (enqueueEvent . ERPC)
      . deser

verifyWrappedRPC :: PublicKey -> RPC -> Bool
verifyWrappedRPC k rpc = case rpc of
  AE ae          -> verifyRPC k ae
  AER aer        -> verifyRPC k aer
  RV rv          -> verifyRPC k rv
  RVR rvr        -> verifyRPC k rvr
  CMD cmd        -> verifyRPC k cmd
  CMDR cmdr      -> verifyRPC k cmdr
  REVOLUTION rev -> verifyRPC k rev
  DBG _          -> True

senderId :: RPC -> Maybe NodeID
senderId rpc = case rpc of
    AE ae          -> Just (_leaderId ae)
    AER aer        -> Just (_aerNodeId aer)
    RV rv          -> Just (_rvCandidateId rv)
    RVR rvr        -> Just (_rvrNodeId rvr)
    CMD cmd        -> Just (_cmdClientId cmd)
    CMDR cmdr      -> Just (_cmdrNodeId cmdr)
    REVOLUTION rev -> Just (_revClientId rev)
    DBG _          -> Nothing

verifyRPCWithKey :: (Monad m) => RPC -> Raft m Bool
verifyRPCWithKey rpc =
  case rpc of
    AE _   -> doVerify rpc
    AER _  -> doVerify rpc
    RV _   -> doVerify rpc
    RVR _  -> doVerify rpc
    CMDR _ -> doVerify rpc
    _      -> return False
  where
    doVerify rpc' = do
      pks <- view (cfg.publicKeys)
      let mk = (\k -> Map.lookup k pks) =<< senderId rpc'
      maybe
        (debug "RPC has invalid signature" >> return False)
        (\k -> return (verifyWrappedRPC k rpc'))
        mk

verifyRPCWithClientKey :: Monad m => RPC -> Raft m Bool
verifyRPCWithClientKey rpc =
  case rpc of
    CMD _        -> doVerify rpc
    REVOLUTION _ -> doVerify rpc
    _            -> return False
  where
    doVerify rpc' = do
      pks <- view (cfg.clientPublicKeys)
      let mk = (\k -> Map.lookup k pks) =<< senderId rpc'
      maybe
        (debug "RPC has invalid signature" >> return False)
        (\k -> return (verifyWrappedRPC k rpc'))
        mk

signRPCWithKey :: (Monad m, Serialize rpc, HasSig rpc) => rpc -> Raft m rpc
signRPCWithKey rpc = do
  pk <- view (cfg.privateKey)
  return (signRPC pk rpc)

updateTerm :: Monad m => Term -> Raft m ()
updateTerm t = do
  void $ rs.writeTermNumber ^$ t
  term .= t
