module Network.Tangaroa.Timer
  ( resetElectionTimer
  , resetHeartbeatTimer
  , cancelTimer
  ) where

import Control.Lens hiding (Index)
import Control.Monad.Trans (lift)
import System.Random
import Control.Concurrent.Lifted

import Network.Tangaroa.Types
import Network.Tangaroa.Util

getNewElectionTimeout :: Raft nt et rt mt Int
getNewElectionTimeout = view (cfg.electionTimeoutRange) >>= lift . randomRIO

resetElectionTimer :: Raft nt et rt mt ()
resetElectionTimer = do
  timeout <- getNewElectionTimeout
  setTimedEvent (ElectionTimeout $ show (timeout `div` 1000) ++ "ms") timeout

resetHeartbeatTimer :: Raft nt et rt mt ()
resetHeartbeatTimer = do
  timeout <- view (cfg.heartbeatTimeout)
  setTimedEvent (HeartbeatTimeout $ show (timeout `div` 1000) ++ "ms") timeout

wait :: Int -> Raft nt et rt mt ()
wait t = threadDelay t

-- | Cancel any existing timer.
cancelTimer :: Raft nt et rt mt ()
cancelTimer = do
  use timerThread >>= maybe (return ()) killThread
  timerThread .= Nothing

-- | Cancels any pending timer and sets a new timer to trigger an event after t
-- microseconds.
setTimedEvent :: Event nt et rt -> Int -> Raft nt et rt mt ()
setTimedEvent e t = do
  cancelTimer
  tmr <- fork $ wait t >> enqueueEvent e
  timerThread .= Just tmr
