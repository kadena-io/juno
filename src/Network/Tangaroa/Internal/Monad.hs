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
  , cancelTimer
  , setTimedEvent
  , sendAppendEntries
  , sendAppendEntriesResponse
  , handleAppendEntries
  , handleAppendEntriesResponse
  , handleRequestVote
  , handleRequestVoteResponse
  , handleCommand
  , (^>>=)
  , (^$)
  ) where

import Control.Lens hiding (Index)

import Network.Tangaroa.Types
import Network.Tangaroa.Internal.State

import Control.Monad.Fork.Class (fork)

import Control.Concurrent (threadDelay, killThread)
import Control.Concurrent.Chan.Unagi
import Control.Monad.RWS

type Raft nt et rt mt ht a = RWST (RaftEnv nt et rt mt ht) () (RaftState nt) IO a

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

becomeCandidate :: Raft nt et rt mt ht ()
becomeCandidate = role .= Candidate initialCandidateState

sendAppendEntries :: nt -> Raft nt et rt mt ht ()
sendAppendEntries = undefined -- TODO
--aeVotes = ()
--if nt not in leader.convincedFollowers:
--  aeVotes = leader.votes
--prevLogIndex, prevLogTerm = last log entry
--commitId = max(logIndex that has 2f success response)
--followerCommits = (2f+1) success response signatures at log index = commitId

handleAppendEntries :: AppendEntries nt et -> Raft nt et rt mt ht ()
handleAppendEntries ae =
  lift $ putStrLn "Got an appendEntries RPC."
-- TODO
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

sendAppendEntriesResponse :: nt -> Raft nt et rt mt ht ()
sendAppendEntriesResponse = undefined -- TODO

handleAppendEntriesResponse :: AppendEntriesResponse -> Raft nt et rt mt ht ()
handleAppendEntriesResponse aer =
  lift $ putStrLn "Got an appendEntriesResponse RPC."
-- TODO

handleRequestVote :: RequestVote nt -> Raft nt et rt mt ht ()
handleRequestVote rv =
  lift $ putStrLn "Got a requestVote RPC."
-- TODO

handleRequestVoteResponse :: RequestVoteResponse -> Raft nt et rt mt ht ()
handleRequestVoteResponse rvr =
  lift $ putStrLn "Got a requestVoteResponse RPC."
-- TODO

handleCommand :: Command et -> Raft nt et rt mt ht ()
handleCommand cmd =
  lift $ putStrLn "Got a command RPC."
-- TODO

--bbind :: MonadReader r m => m a -> m (a -> m b) -> m b
--bbind ma mf = mf >>= (ma >>=)

-- like bind, but the monadic function is a lens with a monadic function
-- as its target
infixl 1 ^>>=
(^>>=) :: forall b s (t :: (* -> *) -> * -> *) (m :: * -> *) a.
  (MonadReader s (t m), MonadTrans t, Monad m) =>
  m a -> Getting (a -> m b) s (a -> m b) -> t m b
(^>>=) ma l = do
  mf <- view l
  lift (ma >>= mf)

-- like $, but the function is a lens with a function as its target
infixr 0 ^$
(^$) :: forall b s (t :: (* -> *) -> * -> *) (m :: * -> *) t1.
  (MonadReader s (t m), MonadTrans t, Monad m) =>
  Getting (t1 -> b) s (t1 -> b) -> t1 -> t m b
(^$) l a = do
  f <- view l
  lift (return (f a))
