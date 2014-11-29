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

-- apply the un-applied log entries up through commitIndex
-- TODO: have this done on a separate thread via event passing
applyLogEntries :: Raft nt et rt mt ht ()
applyLogEntries = do
  la <- use lastApplied
  ci <- use commitIndex
  apply <- view (rs.applyLogEntry)
  le <- use logEntries
  let leToApply = fmap snd . Seq.drop (la + 1) . Seq.take (ci + 1) $ le
  results <- mapM apply leToApply
  -- TODO: send results to client if you are the leader
  lastApplied .= ci

-- TODO: check this
logInfoForNextIndex :: Maybe Index -> Seq (Term,et) -> (Index,Term)
logInfoForNextIndex mni es =
  case mni of
    Just ni -> let pli = ni - 1 in (pli, fst (Seq.index es pli))
    Nothing -> (startIndex, startTerm)

lastLogInfo :: Seq (Term,et) -> (Index,Term)
lastLogInfo es =
  case Seq.viewr es of
    Seq.EmptyR      -> (startIndex, startTerm)
    _ Seq.:> (t, _) -> (Seq.length es - 1, t)

sendAppendEntries :: Ord nt => nt -> Raft nt et rt mt ht ()
sendAppendEntries target = do
  send <- view (rs.sendMessage)
  ser <- view (rs.serializeRPC)
  mni <- use $ lNextIndex.at target
  es <- use logEntries
  let (pli,plt) = logInfoForNextIndex mni es
  ct <- use term
  nid <- view (cfg.nodeId)
  ci <- use commitIndex
  send target $ ser $ AE $
    AppendEntries ct nid pli plt (Seq.drop (pli + 1) es) ci

sendAppendEntriesResponse :: nt -> Bool -> Index -> Raft nt et rt mt ht ()
sendAppendEntriesResponse target success lindex = do
  ct <- use term
  nid <- view (cfg.nodeId)
  send <- view (rs.sendMessage)
  ser <- view (rs.serializeRPC)
  send target $ ser $ AER $ AppendEntriesResponse ct nid success lindex

sendRequestVote :: nt -> Raft nt et rt mt ht ()
sendRequestVote target = do
  ct <- use term
  nid <- view (cfg.nodeId)
  send <- view (rs.sendMessage)
  ser <- view (rs.serializeRPC)
  es <- use logEntries
  let (lli, llt) = lastLogInfo es
  send target $ ser $ RV $ RequestVote ct nid lli llt

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
  es <- use logEntries
  let oldLastEntry = Seq.length es - 1
  let newLastEntry = _prevLogIndex + Seq.length _aeEntries
  if _aeTerm < ct || not plmatch
    then sendAppendEntriesResponse _leaderId False oldLastEntry
    else do
      appendLogEntries _prevLogIndex _aeEntries
      sendAppendEntriesResponse _leaderId True newLastEntry
      nc <- use commitIndex
      when (_leaderCommit > nc) $ do
        commitIndex .= min _leaderCommit newLastEntry
        applyLogEntries

handleAppendEntriesResponse :: Ord nt => AppendEntriesResponse nt -> Raft nt et rt mt ht ()
handleAppendEntriesResponse AppendEntriesResponse{..} = do
  rs.debugPrint ^$ "got an appendEntriesResponse RPC"
  ct <- use term
  when (_aerTerm > ct) $ updateTerm _aerTerm >> becomeFollower
  r <- use role
  when (r == Leader && _aerTerm == ct) $
    if _aerSuccess
      then do
        lMatchIndex.at _aerNodeId .= Just _aerIndex
        lNextIndex .at _aerNodeId .= Just (_aerIndex + 1)
        leaderUpdateCommitIndex
        applyLogEntries
      else do
        lNextIndex %= Map.adjust (subtract 1) _aerNodeId
        fork_ $ sendAppendEntries _aerNodeId

-- called only as leader
-- checks to see what the largest N where a majority of
-- the lMatchIndex set is >= N
leaderUpdateCommitIndex :: Ord nt => Raft nt et rt mt ht ()
leaderUpdateCommitIndex = return () -- TODO
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
