module Juno.Runtime.Timer
  ( resetElectionTimer
  , resetElectionTimerLeader
  , resetHeartbeatTimer
  , resetLastBatchUpdate
  , hasElectionTimerLeaderFired
  , cancelTimer
  ) where

import qualified Data.ByteString as B
import Data.Sequence
import Control.Monad
import Control.Lens hiding (Index)
import Juno.Runtime.Types
import Juno.Util.Util

getNewElectionTimeout :: Monad m => Raft m Int
getNewElectionTimeout = view (cfg.electionTimeoutRange) >>= randomRIO

-- THREAD: SERVER MAIN
resetElectionTimer :: Monad m => Raft m ()
resetElectionTimer = do
  timeout <- getNewElectionTimeout
  setTimedEvent (ElectionTimeout $ show (timeout `div` 1000) ++ "ms") timeout

{-
This one requires a serious comment. The problem this solves is:
  - Given a cluster of size M for which N (M/2 + 1) nodes are required for a quorum.
  - This network begins in the previously in a good state
  - The network then undergoes a full network partition for enough time for all Followers to become Candidates

When the parition ends, only N - 1 Candidate nodes (Followers that timed out during the partition) come back online along with the Leader.
The Candidates and Leader all at the same LogIndex but at possibly different Terms.
This makes the Leader the deciding vote, otherwise the system stays down and cannot make progress.

This makes the question "how can the leader be certian that it has no Followers and thus can either vote for become a Candidate."
The answer is that a Leader can be certian that if 2x the max bound of the ElectionTimeout time is experienced between AER's
then all Followers must have converted to Candidates or Lazily Voted already.

How to use:
  1. Once a node becomes Leader, reset the counter of microseconds via resetElectionTimerLeader
  2. Every time an AER is received by the Leader, in which the node is the designated Leader (and Term is consistent) reset the timer
  2. Every time a heartbeat is encountered, check if the counter is greater than 2x the max electionTimeoutRange
       a. If it is in excess, trigger an election timeout event
       b. otherwise, increase the counter by the heartbeatTimeout amount

If the Leader ever catches an electionTimeout Event (see 2.a):
  - if a lazy vote has been cached, it casts its LazyVote and becomes a Follower (following the Follower's lazy vote codepath)
  - otherwise, convert to a follower and issue RequestVotes (following the Followe's become candidate codepath)
-}
hasElectionTimerLeaderFired :: Monad m => Raft m Bool
hasElectionTimerLeaderFired = do
  -- this isn't pure because the type of both maxTimeout and timeSinceLastAER in state is Int
  -- and I was worried about the arg order being swapped by accident...
  maxTimeout <- ((*2) . snd) <$> view (cfg.electionTimeoutRange)
  timeSinceLastAER' <- use timeSinceLastAER
  return $ timeSinceLastAER' >= maxTimeout

resetElectionTimerLeader :: Monad m => Raft m ()
resetElectionTimerLeader = timeSinceLastAER .= 0

-- THREAD: SERVER MAIN, CLIENT MAIN
resetHeartbeatTimer :: Monad m => Raft m ()
resetHeartbeatTimer = do
  timeout <- view (cfg.heartbeatTimeout)
  setTimedEvent (HeartbeatTimeout $ show (timeout `div` 1000) ++ "ms") timeout

-- | Cancel any existing timer.
-- THREAD: SERVER MAIN, CLIENT MAIN. updates state
cancelTimer :: Monad m => Raft m ()
cancelTimer = do
  tmr <- use timerThread
  case tmr of
    Nothing -> return ()
    Just t -> view (rs.killEnqueued) >>= \f -> f t
  timerThread .= Nothing

-- | Cancels any pending timer and sets a new timer to trigger an event after t
-- microseconds.
-- THREAD: SERVER MAIN, CLIENT MAIN updates state
setTimedEvent :: Monad m => Event -> Int -> Raft m ()
setTimedEvent e t = do
  cancelTimer
  tmr <- enqueueEventLater t e -- forks, no state
  timerThread .= Just tmr

resetLastBatchUpdate :: Monad m => Raft m ()
resetLastBatchUpdate = do
  les <- use logEntries
  latestLogHash <- return $ case viewr les of
    EmptyR -> B.empty
    _ :> LogEntry _ _ h -> h
  curTime <- join $ view (rs.getTimestamp)
  lLastBatchUpdate .= (curTime, latestLogHash)
