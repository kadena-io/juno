{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Juno.Util.Util
  ( seqIndex
  , getQuorumSize
  , debug
  , randomRIO
  , runRWS_
  , enqueueEvent, enqueueEventLater
  , dequeueEvent
  , dequeueCommand
  , logMetric
  , logStaticMetrics
  , setTerm
  , setRole
  , setCurrentLeader
  , updateLNextIndex
  , setLNextIndex
  , getCmdSigOrInvariantError
  , getRevSigOrInvariantError
  ) where

import Juno.Types
import Juno.Util.Combinator

import Control.Lens
import Data.Sequence (Seq)
import Control.Monad.RWS.Strict
import qualified Control.Concurrent.Lifted as CL
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import qualified System.Random as R

seqIndex :: Seq a -> Int -> Maybe a
seqIndex s i =
  if i >= 0 && i < Seq.length s
    then Just (Seq.index s i)
    else Nothing

getQuorumSize :: Int -> Int
getQuorumSize n = 1 + floor (fromIntegral n / 2 :: Float)

debug :: Monad m => String -> Raft m ()
debug s = do
  dbg <- view (rs.debugPrint)
  nid <- view (cfg.nodeId)
  role' <- use nodeRole
  dontDebugFollower' <- view (cfg.dontDebugFollower)
  case role' of
    Leader -> dbg nid $ "\ESC[0;34m[LEADER]\ESC[0m: " ++ s
    Follower -> when (not dontDebugFollower') $ dbg nid $ "\ESC[0;32m[FOLLOWER]\ESC[0m: " ++ s
    Candidate -> dbg nid $ "\ESC[1;33m[CANDIDATE]\ESC[0m: " ++ s

randomRIO :: (Monad m, R.Random a) => (a,a) -> Raft m a
randomRIO rng = view (rs.random) >>= \f -> f rng -- R.randomRIO

runRWS_ :: Monad m => RWST r w s m a -> r -> s -> m ()
runRWS_ ma r s = void $ runRWST ma r s

-- no state update
enqueueEvent :: Monad m => Event -> Raft m ()
enqueueEvent event = view (rs.enqueue) >>= \f -> f event
  -- lift $ writeChan ein event

enqueueEventLater :: Monad m => Int -> Event -> Raft m CL.ThreadId
enqueueEventLater t event = view (rs.enqueueLater) >>= \f -> f t event

-- no state update
dequeueEvent :: Monad m => Raft m Event
dequeueEvent = join $ view (rs.dequeue)

-- dequeue command from API interface
dequeueCommand :: MonadIO m => Raft m (RequestId, [CommandEntry])
dequeueCommand = join $ view (rs.dequeueFromApi)

logMetric :: Monad m => Metric -> Raft m ()
logMetric metric = view (rs.publishMetric) >>= \f -> f metric

logStaticMetrics :: Monad m => Raft m ()
logStaticMetrics = do
  logMetric . MetricNodeId =<< view (cfg.nodeId)
  logMetric . MetricClusterSize =<< view clusterSize
  logMetric . MetricQuorumSize =<< view quorumSize


setTerm :: Monad m => Term -> Raft m ()
setTerm t = do
  void $ rs.writeTermNumber ^$ t
  term .= t
  logMetric $ MetricTerm t

setRole :: Monad m => Role -> Raft m ()
setRole newRole = do
  nodeRole .= newRole
  logMetric $ MetricRole newRole

setCurrentLeader :: Monad m => Maybe NodeID -> Raft m ()
setCurrentLeader mNode = do
  currentLeader .= mNode
  logMetric $ MetricCurrentLeader mNode

updateLNextIndex :: Monad m
                 => (Map.Map NodeID LogIndex -> Map.Map NodeID LogIndex)
                 -> Raft m ()
updateLNextIndex f = do
  lNextIndex %= f
  lni <- use lNextIndex
  ci <- use commitIndex
  logMetric $ MetricAvailableSize $ availSize lni ci

  where
    -- | The number of nodes at most one behind the commit index
    availSize lni ci = let oneBehind = pred ci
                       in succ $ Map.size $ Map.filter (>= oneBehind) lni

setLNextIndex :: Monad m
              => Map.Map NodeID LogIndex
              -> Raft m ()
setLNextIndex = updateLNextIndex . const

getCmdSigOrInvariantError :: String -> Command -> Signature
getCmdSigOrInvariantError where' s@Command{..} = case _cmdProvenance of
  NewMsg -> error $ where'
    ++ ": This should be unreachable, somehow an AE got through with a LogEntry that contained an unsigned Command" ++ show s
  ReceivedMsg{..} -> _digSig _pDig

getRevSigOrInvariantError :: String -> Revolution -> Signature
getRevSigOrInvariantError where' s@Revolution{..} = case _revProvenance of
  NewMsg -> error $ where'
    ++ ": This should be unreachable, got an unsigned Revolution" ++ show s
  ReceivedMsg{..} -> _digSig _pDig
