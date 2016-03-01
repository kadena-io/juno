module Juno.Runtime.Timer
  ( resetElectionTimer
  , resetHeartbeatTimer
  , cancelTimer
  ) where

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
