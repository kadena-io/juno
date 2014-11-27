{-# LANGUAGE TemplateHaskell #-}

module Network.Tangaroa.Internal.Monad
  ( Raft
  , RaftEnv(..), cfg, conn, eventIn, eventOut
  , fork
  , sendEvent
  , becomeFollower
  , becomeLeader
  , becomeCandidate
  , incrementCommitIndex
  , cancelTimer
  , setTimedEvent
  , sendAppendEntries
  , sendAppendEntriesResponse
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

incrementCommitIndex :: Raft nt mt ht ()
incrementCommitIndex = undefined -- TODO
--N = self.commitIndex+1
--while true:
--  count = 0
--  for matchIndex in self.matchIndex:
--    if matchIndex >= N:
--      count++
--  if self.log[N].term == self.term and count >= quorum_size:
--    N = N+1
--  else:
--    break
--self.commitIndex = N-1

sendAppendEntries :: RaftSpec nt et rt mt ht -> nt -> Raft nt mt ht ()
sendAppendEntries = undefined -- TODO
--AppendEntries ae
--ae._aeTerm = self.term
--ae._leaderId = self.nodeId
--ae._prevLogIndex = self.nextIndex[nt] - 1
--ae._prevLogTerm = self.log[ae._prevLogIndex].term
--ae._entries = self.log[ae._prevLogIndex+1:]
--ae._leaderCommit = self.commitIndex
--send ae

handleAppendEntries :: RaftSpec nt et rt mt ht -> AppendEntries nt et -> Raft nt mt ht ()
handleAppendEntries = undefined -- TODO
--AppendEntriesResponse aer
--aer._aerTerm = self.term
--if ae._aeTerm < self.term:
--  aer._success = false
--if self.log[ae._prevLogIndex] == null
--  or self.log[_prevLogIndex].term != ae._prevLogTerm:
--  aer._success = false
--for entry in ae._entries:
--  existingEntry = self.log[entry.index]
--  if existingEntry != null and existingEntry.term != entry.term:
--    delete self.log[entry.index:]
--for entry in ae._entries:
--  if self.log[entry.index] == null:
--    self.log[entry.index] = entry
--commitIndex = min(ae._leaderCommit, ae._entries[-1].index)
--for entry in self.log[self.commitIndex+1:commitIndex]:
--  commit entry
--self.commitIndex = commitIndex

sendAppendEntriesResponse :: RaftSpec nt et rt mt ht -> nt -> Raft nt mt ht ()
sendAppendEntriesResponse = undefined -- TODO

handleAppendEntriesResponse :: RaftSpec nt et rt mt ht -> AppendEntriesResponse -> Raft nt mt ht ()
handleAppendEntriesResponse = undefined -- TODO
--AppendEntries ae
--AppendEntriesResponse aer
--if aer._aerTerm > self.term:
--  self.term = aer._aerTerm
--  self.becomeFollower
--else:
--  if aer._success:
--    self.nextIndex[nt] = ae._prevLogIndex+1+len(ae._entries)
--    self.matchIndex[nt] = self.nextIndex[nt]
--    self.incrementCommitIndex
--  else:
--    self.nextIndex[nt] -= 1
--    retry AppendEntries