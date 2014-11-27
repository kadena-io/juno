{-# LANGUAGE TemplateHaskell #-}

module Network.Tangaroa.Internal.Monad
  ( Raft
  , RaftEnv(..), cfg, conn, eventIn, eventOut
  , fork
  , sendEvent
  , becomeFollower
  , becomeLeader
  , becomeCandidate
  , cancelTimer
  , setTimedEvent
  , sendAppendEntries
  , handleAppendEntries
  , handleAppendEntriesResponse
  ) where

import Control.Lens hiding (Index)

import Network.Tangaroa.Types
import Network.Tangaroa.Spec
import Network.Tangaroa.Internal.State

import Control.Monad.Fork.Class (fork)

import Control.Concurrent (threadDelay, killThread)
import Control.Concurrent.Chan.Unagi
import Control.Monad.RWS

type Raft nt mt ht a = RWST (RaftEnv nt mt ht) () (RaftState nt) IO a

sendEvent :: Event mt -> Raft nt mt ht ()
sendEvent event = do
  ein <- view eventIn
  lift $ writeChan ein event

wait :: Int -> Raft nt mt ht ()
wait t = lift (threadDelay t)

-- | Cancel any exiting timer.
cancelTimer :: Raft nt mt ht ()
cancelTimer = do
  use timerThread >>= maybe (return ()) (lift . killThread)
  timerThread .= Nothing

-- | Cancels any pending timer and sets a new timer to trigger an event after t
-- microseconds.
setTimedEvent :: Event mt -> Int -> Raft nt mt ht ()
setTimedEvent e t = do
  cancelTimer
  tmr <- fork $ wait t >> sendEvent e
  timerThread .= Just tmr

becomeFollower :: Raft nt mt ht ()
becomeFollower = role .= Follower

becomeLeader :: Raft nt mt ht ()
becomeLeader = role .= Leader initialLeaderState

becomeCandidate :: Raft nt mt ht ()
becomeCandidate = role .= Candidate initialCandidateState

sendAppendEntries :: RaftSpec nt et rt mt ht -> nt -> Raft nt mt ht ()
sendAppendEntries = undefined -- TODO

handleAppendEntries :: RaftSpec nt et rt mt ht -> AppendEntries nt et -> Raft nt mt ht ()
handleAppendEntries = undefined -- TODO

handleAppendEntriesResponse :: RaftSpec nt et rt mt ht -> AppendEntriesResponse -> Raft nt mt ht ()
handleAppendEntriesResponse = undefined -- TODO
