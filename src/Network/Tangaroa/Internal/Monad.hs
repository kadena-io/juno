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

sendAppendEntries :: RaftSpec nt et rt mt ht -> nt -> Raft nt mt ht ()
sendAppendEntries = undefined -- TODO
--aeVotes = ()
--if nt not in leader.convincedFollowers:
--  aeVotes = leader.votes
--prevLogIndex, prevLogTerm = last log entry
--commitId = max(logIndex that has 2f success response)
--followerCommits = (2f+1) success response signatures at log index = commitId

handleAppendEntries :: RaftSpec nt et rt mt ht -> AppendEntries nt et -> Raft nt mt ht ()
handleAppendEntries = undefined -- TODO
--success = false if invalidSig(termSignature)
--success = false if aeTerm < self.term
--success = false if aeVotes == null and termCertificates[aeTerm] == null
--self.term = max(aeTerm, self.term)
--termCertificates[self.term] = aeVotes if aeVotes != null

--success = false if invalidMsg(logEntry) for any logEntry in entries
--success = false if log[prevLogIndex] == null or logTerm[prevLogIndex] != prevLogTerm
--success = false if invalidSig(followerCommit) for any followerCommit in followerCommits
--if success:
--  firstConflictEntry = min(entries with same index but different term)
--  delete firstConflictEntry...end_of_log
--  sendResultRPC, success = true for each entry in log



sendAppendEntriesResponse :: RaftSpec nt et rt mt ht -> nt -> Raft nt mt ht ()
sendAppendEntriesResponse = undefined -- TODO

handleAppendEntriesResponse :: RaftSpec nt et rt mt ht -> AppendEntriesResponse -> Raft nt mt ht ()
handleAppendEntriesResponse = undefined -- TODO
