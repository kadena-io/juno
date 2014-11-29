{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module Network.Tangaroa.Internal.Monad
  ( Raft
  , RaftEnv(..), cfg, conn, eventIn, eventOut
  , initialRaftState
  , fork, fork_
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
  , ($^)
  , (^$^)
  , (>>=^)
  , (^>>=^)
  , (>>=$)
  ) where

import Prelude hiding (mapM)
import Control.Applicative
import Control.Concurrent (threadDelay, killThread)
import Control.Concurrent.Chan.Unagi
import Control.Lens hiding (Index)
import Control.Monad.Fork.Class
import Control.Monad.RWS hiding (mapM)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Traversable (mapM)

import Network.Tangaroa.Types

import System.Random

sendEvent :: Event mt -> Raft nt et rt mt ht ()
sendEvent event = do
  ein <- view eventIn
  lift $ writeChan ein event

wait :: Int -> Raft nt et rt mt ht ()
wait t = lift (threadDelay t)

-- | Cancel any existing timer.
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

initialRaftState :: RaftState nt et
initialRaftState = RaftState
  Follower startTerm Seq.empty startIndex startIndex Nothing
  Map.empty Set.empty Set.empty Map.empty Map.empty

becomeFollower :: Raft nt et rt mt ht ()
becomeFollower = role .= Follower

becomeLeader :: Raft nt et rt mt ht ()
becomeLeader = role .= Leader
--send initial heartbeat

becomeCandidate :: Raft nt et rt mt ht ()
becomeCandidate = role .= Candidate
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
applyLogEntries = do
  la <- use lastApplied
  ci <- use commitIndex
  apply <- view (rs.applyLogEntry)
  le <- use logEntries
  let leToApply = fmap snd . Seq.drop (la + 1) . Seq.take (ci + 1) $ le
  _ <- mapM apply leToApply
  return ()
-- TODO: apply un-applied log entries up through commitIndex
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

sendAppendEntriesResponse :: nt -> AppendEntriesResponse -> Raft nt et rt mt ht ()
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

getNewElectionTimeout :: Raft nt et rt mt ht Int
getNewElectionTimeout = view (cfg.electionTimeoutRange) >>= lift . randomRIO

resetElectionTimer :: Raft nt et rt mt ht ()
resetElectionTimer = do
  timeout <- getNewElectionTimeout
  setTimedEvent (Election $ show (timeout `div` 1000) ++ "ms") timeout

hasMatchingPrevLogEntry :: Index -> Term -> Raft nt et rt mt ht Bool
hasMatchingPrevLogEntry pli plt = do
  es <- use logEntries
  return $ pli >= Seq.length es && fst (Seq.index es pli) == plt

updateTerm :: Term -> Raft nt et rt mt ht ()
updateTerm t = do
  rs.writeTermNumber ^$ t
  term .= t

appendLogEntries :: Index -> Seq (Term,et) -> Raft nt et rt mt ht ()
appendLogEntries pli es = return () -- TODO: append entries to the log starting with the one after pli

fork_ :: (Monad m, MonadFork m) => m () -> m ()
fork_ a = fork a >>= return . const ()

handleAppendEntries :: AppendEntries nt et -> Raft nt et rt mt ht ()
handleAppendEntries AppendEntries{..} = do
  rs.debugPrint ^$ "got an appendEntries RPC"
  ct <- use term
  when (_aeTerm > ct) $ updateTerm _aeTerm >> becomeFollower
  when (_aeTerm == ct) resetElectionTimer
  plmatch <- hasMatchingPrevLogEntry _prevLogIndex _prevLogTerm
  if _aeTerm < ct || not plmatch
    then sendAppendEntriesResponse _leaderId $ AppendEntriesResponse ct False
    else do
      appendLogEntries _prevLogIndex _aeEntries
      nc <- use commitIndex
      when (_leaderCommit > nc) $
        commitIndex .= min _leaderCommit (_prevLogIndex + Seq.length _aeEntries)
      fork_ applyLogEntries

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

-- like $, but the function is a lens from the reader environment with a
-- pure function as its target
infixr 2 ^$
(^$) :: forall (m :: * -> *) b r a. (MonadReader r m, Functor m) =>
  Getting (a -> b) r (a -> b) -> a -> m b
lf ^$ a = fmap ($ a) (view lf)

infixr 2 $^
($^) :: forall (m :: * -> *) b r a. (MonadReader r m, Functor m) =>
  (a -> b) -> Getting a r a -> m b
f $^ la = fmap f (view la)

infixr 2 ^$^
(^$^) :: forall (m :: * -> *) b r t. (MonadReader r m, Applicative m) =>
  Getting (t -> b) r (t -> b) -> Getting t r t -> m b
lf ^$^ la = view lf <*> view la

-- like bind, but the monadic function is a lens from the reader environment of
-- the same monad
infixl 1 >>=^
(>>=^) :: forall (m :: * -> *) b r a. MonadReader r m =>
  m a -> Getting (a -> m b) r (a -> m b) -> m b
ma >>=^ lf = view lf >>= (ma >>=)

-- like the above, except both the function and the argument are reader lenses
(^>>=^) :: forall (m :: * -> *) b r a. MonadReader r m =>
  Getting a r a -> Getting (a -> m b) r (a -> m b) -> m b
infixl 1 ^>>=^
la ^>>=^ lf = view lf >>= (view la >>=)

-- apply a monadic function that's wrapped in the same monad to a pure value
infixl 1 >>=$
(>>=$) :: Monad m => m (a -> m b) -> a -> m b
mmf >>=$ x = mmf >>= ($ x)

-- lets you do things like:
-- rs.sendMessage ^$^ cfg.nodeId >>=$ m
