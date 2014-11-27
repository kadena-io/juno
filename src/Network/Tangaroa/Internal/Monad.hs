{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module Network.Tangaroa.Internal.Monad
  ( Raft
  , RaftEnv(..), cfg, conn, eventIn, eventOut
  , fork
  , sendEvent
  , wait
  , becomeFollower
  , becomeLeader
  , becomeCandidate
  , incrementCommitIndex
  , applyLogEntries
  , cancelTimer
  , setTimedEvent
  , sendAppendEntries
  , sendAppendEntriesResponse
  , sendRequestVote
  , sendRequestVoteResponse
  , handleAppendEntries
  , handleAppendEntriesResponse
  , handleRequestVote
  , handleRequestVoteResponse
  , handleCommand
  , (^$)
  , (>>=^)
  , (^>>=^)
  ) where

import Control.Concurrent (threadDelay, killThread)
import Control.Concurrent.Chan.Unagi
import Control.Lens hiding (Index)
import Control.Monad.Fork.Class (fork)
import Control.Monad.RWS

import Network.Tangaroa.Internal.State
import Network.Tangaroa.Types

sendEvent :: Event mt -> Raft nt et rt mt ht ()
sendEvent event = do
  ein <- view eventIn
  lift $ writeChan ein event

wait :: Int -> Raft nt et rt mt ht ()
wait t = lift (threadDelay t)

-- | Cancel any exiting timer.
cancelTimer :: Raft nt et rt mt ht ()
cancelTimer = do
  use timerThread >>= maybe (return ()) (lift . killThread)
  timerThread .= Nothing

-- | Cancels any pending timer and sets a new timer to trigger an event after t
-- microseconds.
setTimedEvent :: Event mt -> Int -> Raft nt et rt mt ht ()
setTimedEvent e t = do
  cancelTimer
  tmr <- fork $ wait t >> sendEvent e
  timerThread .= Just tmr

becomeFollower :: Raft nt et rt mt ht ()
becomeFollower = role .= Follower

becomeLeader :: Raft nt et rt mt ht ()
becomeLeader = role .= Leader initialLeaderState
--send initial heartbeat

becomeCandidate :: Raft nt et rt mt ht ()
becomeCandidate = role .= Candidate initialCandidateState
--self._votes.add(self)
--send RequestVote RPC

incrementCommitIndex :: Raft nt et rt mt ht ()
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

applyLogEntries :: Raft nt et rt mt ht ()
applyLogEntries = undefined -- TODO
--while self.commitIndex > self.lastApplied:
--  self.lastApplied++
--  commit self.log[self.lastApplied]

sendAppendEntries :: nt -> Raft nt et rt mt ht ()
sendAppendEntries = undefined -- TODO
--AppendEntries ae
--ae._aeTerm = self.term
--ae._leaderId = self.nodeId
--ae._prevLogIndex = self.nextIndex[nt] - 1
--ae._prevLogTerm = self.log[ae._prevLogIndex].term
--ae._entries = self.log[ae._prevLogIndex+1:]
--ae._leaderCommit = self.commitIndex
--send ae

sendAppendEntriesResponse :: nt -> Raft nt et rt mt ht ()
sendAppendEntriesResponse = undefined -- TODO

sendRequestVote :: nt -> Raft nt et rt mt ht ()
sendRequestVote = undefined -- TODO
--RequestVote rv
--rv._rvTerm = self.term
--rv._candidateId = self.nodeId
--rv._lastLogIndex = self.log[-1].index
--rv._lastLogTerm = self.log[-1].term

sendRequestVoteResponse :: nt -> Raft nt et rt mt ht ()
sendRequestVoteResponse = undefined -- TODO

handleAppendEntries :: AppendEntries nt et -> Raft nt et rt mt ht ()
handleAppendEntries ae =
  lift $ putStrLn "Got an appendEntries RPC."
--AppendEntriesResponse aer
--if role != Follower and ae._aeTerm > self.term:
--  self.becomeFollower
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
--self.commitIndex = min(ae._leaderCommit, ae._entries[-1].index)
--self.applyLogEntries

handleAppendEntriesResponse :: AppendEntriesResponse -> Raft nt et rt mt ht ()
handleAppendEntriesResponse aer =
  lift $ putStrLn "Got an appendEntriesResponse RPC."
-- TODO
--AppendEntries ae
--AppendEntriesResponse aer
--if role != Follower and aer._aerTerm > self.term:
--  self.term = aer._aerTerm
--  self.becomeFollower
--else:
--  if aer._success:
--    self.nextIndex[nt] = ae._prevLogIndex+1+len(ae._entries)
--    self.matchIndex[nt] = self.nextIndex[nt]
--    self.incrementCommitIndex
--    self.applyLogEntries
--  else:
--    self.nextIndex[nt] -= 1
--    retry AppendEntries

handleRequestVote :: RequestVote nt -> Raft nt et rt mt ht ()
handleRequestVote rv =
  lift $ putStrLn "Got a requestVote RPC."
-- TODO
--RequestVoteResponse rvr
--if role != Follower and rv._rvTerm > self.term:
--  self.becomeFollower
--rvr._rvrTerm = self.term
--if rv._rvTerm < self.term:
--  rvr._voteGranted = false
--if (self.votedFor == null or self.votedFor == rv._candidateId)
--  and (rv._lastLogIndex >= self.log[-1].index and rv._lastLogTerm >= self.log[-1].term)
--  rvr._voteGranted = true
--else:
--  rvr._voteGranted = false


handleRequestVoteResponse :: RequestVoteResponse -> Raft nt et rt mt ht ()
handleRequestVoteResponse rvr =
  lift $ putStrLn "Got a requestVoteResponse RPC."
-- TODO
--if role != Follower and rvr._rvrTerm > self.term:
--  self.term = rvr._aerTerm
--  self.becomeFollower --else:
--  if rvr._voteGranted:
--    self._votes.add(nt)
--    if self._votes.size() >= quorum_size:
--      self.becomeLeader

handleCommand :: Command et -> Raft nt et rt mt ht ()
handleCommand cmd =
  lift $ putStrLn "Got a command RPC."
-- TODO

--bbind :: MonadReader r m => m a -> m (a -> m b) -> m b
--bbind ma mf = mf >>= (ma >>=)

-- like bind, but the monadic function is a lens from the reader environment of
-- the same monad
infixl 1 >>=^
(>>=^) :: forall (m :: * -> *) b s a. MonadReader s m =>
  m a -> Getting (a -> m b) s (a -> m b) -> m b
ma >>=^ lf = view lf >>= (ma >>=)

-- like the above, except both the function and the argument are reader lenses
(^>>=^) :: forall (m :: * -> *) b s a. MonadReader s m =>
  Getting a s a -> Getting (a -> m b) s (a -> m b) -> m b
infixl 1 ^>>=^
la ^>>=^ lf = view lf >>= (view la >>=)

-- like $, but the function is a lens from the reader environment with a
-- pure function as its target
infixr 0 ^$
(^$) :: forall (m :: * -> *) b s a. (MonadReader s m, Functor m) =>
  Getting (a -> b) s (a -> b) -> a -> m b
l ^$ a = fmap ($ a) (view l)
