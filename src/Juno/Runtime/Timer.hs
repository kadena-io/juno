module Juno.Runtime.Timer
  ( resetElectionTimer
  , resetElectionTimerLeader
  , resetHeartbeatTimer
  , resetLastBatchUpdate
  , hasElectionTimerLeaderFired
  , cancelTimer
  ) where

import Control.Monad
import Control.Lens hiding (Index)
import Juno.Types
import Juno.Util.Util

getNewElectionTimeout :: Monad m => Raft m Int
getNewElectionTimeout = view (cfg.electionTimeoutRange) >>= randomRIO

resetElectionTimer :: Monad m => Raft m ()
resetElectionTimer = do
  timeout <- getNewElectionTimeout
  setTimedEvent (ElectionTimeout $ show (timeout `div` 1000) ++ "ms") timeout

hasElectionTimerLeaderFired :: Monad m => Raft m Bool
hasElectionTimerLeaderFired = do
  maxTimeout <- ((*2) . snd) <$> view (cfg.electionTimeoutRange)
  timeSinceLastAER' <- use timeSinceLastAER
  return $ timeSinceLastAER' >= maxTimeout

resetElectionTimerLeader :: Monad m => Raft m ()
resetElectionTimerLeader = timeSinceLastAER .= 0

resetHeartbeatTimer :: Monad m => Raft m ()
resetHeartbeatTimer = do
  timeout <- view (cfg.heartbeatTimeout)
  setTimedEvent (HeartbeatTimeout $ show (timeout `div` 1000) ++ "ms") timeout

cancelTimer :: Monad m => Raft m ()
cancelTimer = do
  tmr <- use timerThread
  case tmr of
    Nothing -> return ()
    Just t -> view (rs.killEnqueued) >>= \f -> f t
  timerThread .= Nothing

setTimedEvent :: Monad m => Event -> Int -> Raft m ()
setTimedEvent e t = do
  cancelTimer
  tmr <- enqueueEventLater t e -- forks, no state
  timerThread .= Just tmr

resetLastBatchUpdate :: Monad m => Raft m ()
resetLastBatchUpdate = do
  curTime <- join $ view (rs.getTimestamp)
  l <- lastEntry <$> use logEntries
  lLastBatchUpdate .= (curTime, _leHash <$> l)
