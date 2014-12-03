{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Network.Tangaroa.Internal.Monad
  ( initialRaftState
  , fork
  , fork_
  , sendEvent
  , handleEvents
  , wait
  , becomeFollower
  , becomeLeader
  , becomeCandidate
  , otherNodes
  , applyLogEntries
  , cancelTimer
  , setTimedEvent
  , resetElectionTimer
  , resetHeartbeatTimer
  , debug
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
  , (^>>=)
  , (>>=^)
  , (^>>=^)
  , (>>=$)
  ) where

import Control.Applicative
import Control.Concurrent (threadDelay, killThread)
import Control.Concurrent.Chan.Unagi
import Control.Lens hiding (Index)
import Control.Monad.Fork.Class
import Control.Monad.RWS hiding (mapM)
import Data.Foldable (traverse_)
import Data.Sequence (Seq)
import Data.Traversable (mapM)
import Prelude hiding (mapM)
import System.Random
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Network.Tangaroa.Types


sendEvent :: Event mt -> Raft nt et rt mt ()
sendEvent event = do
  ein <- view eventIn
  lift $ writeChan ein event

wait :: Int -> Raft nt et rt mt ()
wait t = lift (threadDelay t)

-- | Cancel any existing timer.
cancelTimer :: Raft nt et rt mt ()
cancelTimer = do
  use timerThread >>= maybe (return ()) (lift . killThread)
  timerThread .= Nothing

-- | Cancels any pending timer and sets a new timer to trigger an event after t
-- microseconds.
setTimedEvent :: Event mt -> Int -> Raft nt et rt mt ()
setTimedEvent e t = do
  cancelTimer
  tmr <- fork $ wait t >> sendEvent e
  timerThread .= Just tmr

initialRaftState :: RaftState nt et
initialRaftState = RaftState
  Follower   -- role
  startTerm  -- term
  Nothing    -- votedFor
  Nothing    -- currentLeader
  Seq.empty  -- log
  startIndex -- commitIndex
  startIndex -- lastApplied
  Nothing    -- timerThread
  Set.empty  -- cYesVotes
  Set.empty  -- cNoVotes
  Set.empty  -- cUndecided
  Map.empty  -- lNextIndex
  Map.empty  -- lMatchIndex

setVotedFor :: Maybe nt -> Raft nt et rt mt ()
setVotedFor mvote = do
  rs.writeVotedFor ^$ mvote
  votedFor .= mvote

becomeFollower :: Raft nt et rt mt ()
becomeFollower = do
  debug "becoming follower"
  role .= Follower
  resetElectionTimer

becomeLeader :: Ord nt => Raft nt et rt mt ()
becomeLeader = do
  debug "becoming leader"
  role .= Leader
  (currentLeader .=) . Just =<< view (cfg.nodeId)
  ni    <- Seq.length <$> use logEntries
  nlist <- Set.toList <$> view (cfg.otherNodes)
  lNextIndex  .= Map.fromList (map (,ni)         nlist)
  lMatchIndex .= Map.fromList (map (,startIndex) nlist)
  fork_ $ traverse_ sendAppendEntries nlist
  resetHeartbeatTimer

bumpTerm :: Raft nt et rt mt ()
bumpTerm = do
  term %= succTerm
  term .>>=^ rs.writeTermNumber

becomeCandidate :: Ord nt => Raft nt et rt mt ()
becomeCandidate = do
  debug "becoming candidate"
  role .= Candidate
  nid <- view (cfg.nodeId)
  bumpTerm
  setVotedFor $ Just nid
  cYesVotes .= Set.singleton nid -- vote for yourself
  (cUndecided .=) =<< view (cfg.otherNodes)
  cNoVotes .= Set.empty
  -- this is necessary for a single-node cluster
  checkElection
  r <- use role
  when (r == Candidate) $ do
    fork_ $ traverse_ sendRequestVote =<< use cUndecided
    resetHeartbeatTimer

applyCommand :: Command nt et -> Raft nt et rt mt (rt,nt)
applyCommand Command{..} = do
  apply <- view (rs.applyLogEntry)
  result <- apply _entry
  return (result, _clientId)

sendResults :: Seq (rt,nt) -> Raft nt et rt mt ()
sendResults results =
  traverse_ (\(result,target) -> sendRPC target $ CMDR result) results

-- apply the un-applied log entries up through commitIndex
-- and send results to the client if you are the leader
-- TODO: have this done on a separate thread via event passing
applyLogEntries :: Raft nt et rt mt ()
applyLogEntries = do
  la <- use lastApplied
  ci <- use commitIndex
  le <- use logEntries
  let leToApply = fmap (^. _2) . Seq.drop (la + 1) . Seq.take (ci + 1) $ le
  results <- mapM applyCommand leToApply
  r <- use role
  when (r == Leader) $ sendResults results
  lastApplied .= ci

-- TODO: check this
-- called by leaders sending appendEntries.
-- given a replica's nextIndex, get the index and term to send as
-- prevLog(Index/Term)
logInfoForNextIndex :: Maybe Index -> Seq (Term,et) -> (Index,Term)
logInfoForNextIndex mni es =
  case mni of
    Just ni -> let pli = ni - 1 in
      case seqIndex es pli of
        Just (t,_) -> (pli, t)
        Nothing -> (startIndex, startTerm) -- this shouldn't happen
    Nothing -> (startIndex, startTerm)

lastLogInfo :: Seq (Term,et) -> (Index,Term)
lastLogInfo es =
  case Seq.viewr es of
    Seq.EmptyR      -> (startIndex, startTerm)
    _ Seq.:> (t, _) -> (Seq.length es - 1, t)

sendAppendEntries :: Ord nt => nt -> Raft nt et rt mt ()
sendAppendEntries target = do
  mni <- use $ lNextIndex.at target
  es <- use logEntries
  let (pli,plt) = logInfoForNextIndex mni es
  ct <- use term
  nid <- view (cfg.nodeId)
  ci <- use commitIndex
  debug $ "sendAppendEntries: " ++ show ct
  sendRPC target $ AE $
    AppendEntries ct nid pli plt (Seq.drop (pli + 1) es) ci

sendAppendEntriesResponse :: nt -> Bool -> Index -> Raft nt et rt mt ()
sendAppendEntriesResponse target success lindex = do
  ct <- use term
  nid <- view (cfg.nodeId)
  debug $ "sendAppendEntriesResponse: " ++ show ct
  sendRPC target $ AER $ AppendEntriesResponse ct nid success lindex

sendRequestVote :: nt -> Raft nt et rt mt ()
sendRequestVote target = do
  ct <- use term
  nid <- view (cfg.nodeId)
  es <- use logEntries
  let (lli, llt) = lastLogInfo es
  debug $ "sendRequestVote: " ++ show ct
  sendRPC target $ RV $ RequestVote ct nid lli llt

sendRequestVoteResponse :: nt -> Bool -> Raft nt et rt mt ()
sendRequestVoteResponse target vote = do
  ct <- use term
  nid <- view (cfg.nodeId)
  debug $ "sendRequestVoteResponse: " ++ show ct
  sendRPC target $ RVR $ RequestVoteResponse ct nid vote

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

prevLogEntryMatches :: Index -> Term -> Raft nt et rt mt Bool
prevLogEntryMatches pli plt = do
  es <- use logEntries
  case seqIndex es pli of
    -- if we don't have the entry, only return true if pli is startIndex
    Nothing    -> return (pli == startIndex)
    -- if we do have the entry, return true if the terms match
    Just (t,_) -> return (t == plt)

updateTerm :: Term -> Raft nt et rt mt ()
updateTerm t = do
  rs.writeTermNumber ^$ t
  setVotedFor Nothing
  term .= t

getEvent :: Raft nt et rt mt (Event mt)
getEvent = lift . readChan =<< view eventOut

handleEvents :: Ord nt => Raft nt et rt mt ()
handleEvents = forever $ do
  e <- getEvent
  case e of
    Message m          -> handleMessage m
    ElectionTimeout s  -> handleElectionTimeout s
    HeartbeatTimeout s -> handleHeartbeatTimeout s

handleElectionTimeout :: Ord nt => String -> Raft nt et rt mt ()
handleElectionTimeout s = do
  debug $ "election timeout: " ++ s
  becomeCandidate

handleHeartbeatTimeout :: Ord nt => String -> Raft nt et rt mt ()
handleHeartbeatTimeout s = do
  debug $ "heartbeat timeout: " ++ s
  r <- use role
  case r of
    Leader -> do
      traverse_ sendAppendEntries =<< view (cfg.otherNodes)
      resetHeartbeatTimer
    Candidate -> do
      traverse_ sendRequestVote =<< use cUndecided
      -- TODO: have to do this to prevent your voters from timing out
      traverse_ sendRequestVote =<< use cYesVotes
      resetHeartbeatTimer
    Follower ->
      -- This will rarely happen because followers don't get heartbeat timeouts
      -- (becomeFollower cancels it). If we became a follower after the
      -- heartbeat timer places its event on the queue, but before we handle
      -- that event, then we will hit this case, and there's nothing to do but
      -- set the election timer.
      resetElectionTimer

handleMessage :: Ord nt => mt -> Raft nt et rt mt ()
handleMessage m = do
  dm <- rs.deserializeRPC ^$ m
  case dm of
    Just (AE ae)     -> handleAppendEntries ae
    Just (AER aer)   -> handleAppendEntriesResponse aer
    Just (RV rv)     -> handleRequestVote rv
    Just (RVR rvr)   -> handleRequestVoteResponse rvr
    Just (CMD cmd)   -> handleCommand cmd
    Just (CMDR _)    -> lift $ putStrLn "got a command response RPC"
    Just (DBG s)     -> lift $ putStrLn $ "got a debug RPC: " ++ s
    Nothing          -> lift $ putStrLn "failed to deserialize RPC"

-- TODO: check this
appendLogEntries :: Index -> Seq (Term, Command nt et) -> Raft nt et rt mt ()
appendLogEntries pli es =
  logEntries %= (Seq.>< es) . Seq.take (pli + 1)

fork_ :: (Monad m, MonadFork m) => m () -> m ()
fork_ a = fork a >>= return . const ()

handleAppendEntries :: AppendEntries nt et -> Raft nt et rt mt ()
handleAppendEntries AppendEntries{..} = do
  debug $ "got an appendEntries RPC: prev log entry: Index " ++ show _prevLogIndex ++ " " ++ show _prevLogTerm
  ct <- use term
  when (_aeTerm > ct) $ updateTerm _aeTerm >> becomeFollower
  when (_aeTerm >= ct) $ do
    resetElectionTimer
    currentLeader .= Just _leaderId
  plmatch <- prevLogEntryMatches _prevLogIndex _prevLogTerm
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

seqIndex :: Seq a -> Int -> Maybe a
seqIndex s i =
  if i >= 0 && i < Seq.length s
    then Just (Seq.index s i)
    else Nothing

handleAppendEntriesResponse :: Ord nt => AppendEntriesResponse nt -> Raft nt et rt mt ()
handleAppendEntriesResponse AppendEntriesResponse{..} = do
  debug "got an appendEntriesResponse RPC"
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
leaderUpdateCommitIndex :: Ord nt => Raft nt et rt mt ()
leaderUpdateCommitIndex = do
  ci <- use commitIndex
  lmi <- use lMatchIndex
  qsize <- view quorumSize
  ct <- use term
  es <- use logEntries

  -- get all indices in the log past commitIndex and take the ones where the entry's
  -- term is equal to the current term
  let ctinds = filter (\i -> maybe False ((== ct) . fst) (seqIndex es i))
                      [(ci + 1)..(Seq.length es - 1)]

  -- get the prefix of these indices where a quorum of nodes have matching
  -- indices for that entry
  let qcinds = takeWhile (\i -> Map.size (Map.filter (>= i) lmi) >= qsize) ctinds

  when (not $ null qcinds) $ do
    commitIndex .= last qcinds
    debug $ "commit index is now: " ++ show (last qcinds)

-- compare a candidate's prevLog(Index/Term) to our log return true if the
-- candidate's log is at least as up to date as ours measured first by term at
-- the candidate's last log index, and then by log length
candidateLogCompare :: Index -> Term -> Seq (Term,et) -> Bool
candidateLogCompare llIndex llTerm es =
  case seqIndex es llIndex of

    -- we don't have the candidate's log entry
    Nothing -> True

    -- we have an entry, but the candidate's entry has a higher term
    Just (t,_) | t < llTerm -> True

    -- we have an entry with matching term, so return true if the candidate's
    -- last log entry is also the last log entry we have (it will never be
    -- longer in this case, because we have an entry at this index)
    Just (t,_) | t == llTerm -> llIndex == Seq.length es - 1

    -- we have an entry with higher term than the candidate
    _ -> False

handleRequestVote :: Eq nt => RequestVote nt -> Raft nt et rt mt ()
handleRequestVote RequestVote{..} = do
  debug $ "got a requestVote RPC for " ++ show _rvTerm
  ct <- use term
  when (_rvTerm > ct) $ updateTerm _rvTerm >> becomeFollower
  mvote <- use votedFor
  es <- use logEntries
  case mvote of
    _      | _rvTerm < ct -> do
      -- this is an old candidate
      debug "this is for an old term"
      sendRequestVoteResponse _candidateId False

    Just c | c == _candidateId -> do
      -- already voted for this candidate
      debug "already voted for this candidate"
      sendRequestVoteResponse _candidateId True

    Just _ -> do
      -- already voted for a different candidate this term
      debug "already voted for a different candidate"
      sendRequestVoteResponse _candidateId False

    Nothing -> if candidateLogCompare _lastLogIndex _lastLogTerm es
      -- haven't voted yet, so vote for the candidate if it's log is at least as
      -- up to date as ours
      then do
        debug "haven't voted, voting for this candidate"
        setVotedFor (Just _candidateId)
        sendRequestVoteResponse _candidateId True
      else do
        debug "haven't voted, but my log is better than this candidate's"
        sendRequestVoteResponse _candidateId False

debug :: String -> Raft nt et rt mt ()
debug s = view (rs.debugPrint) >>= ($ s)

-- count the yes votes and become leader if you have reached a quorum
checkElection :: Ord nt => Raft nt et rt mt ()
checkElection = do
  nyes <- Set.size <$> use cYesVotes
  qsize <- view quorumSize
  debug $ "yes votes: " ++ show nyes ++ " quorum size: " ++ show qsize
  when (nyes >= qsize) $ becomeLeader

handleRequestVoteResponse :: Ord nt => RequestVoteResponse nt -> Raft nt et rt mt ()
handleRequestVoteResponse RequestVoteResponse{..} = do
  debug $ "got a requestVoteResponse RPC for " ++ show _rvrTerm ++ ": " ++ show _voteGranted
  ct <- use term
  when (_rvrTerm > ct) $ updateTerm _rvrTerm >> becomeFollower
  cUndecided %= Set.delete _rvrNodeId
  if _voteGranted
    then do
      cYesVotes %= Set.insert _rvrNodeId
      checkElection
    else
      cNoVotes %= Set.insert _rvrNodeId

sendRPC :: nt -> RPC nt et rt -> Raft nt et rt mt ()
sendRPC target rpc = do
  send <- view (rs.sendMessage)
  ser <- view (rs.serializeRPC)
  send target $ ser rpc

handleCommand :: Command nt et -> Raft nt et rt mt ()
handleCommand cmd = do
  debug "got a command RPC"
  r <- use role
  ct <- use term
  mlid <- use currentLeader
  case (r, mlid) of
    (Leader, _) ->
      -- we're the leader, so append this to our log with the current term
      logEntries %= (Seq.|> (ct, cmd))
    (_, Just lid) -> do
      -- we're not the leader, but we know who the leader is, so forward this
      -- command
      sendRPC lid $ CMD cmd
    (_, Nothing) ->
      -- we're not the leader, and we don't know who the leader is, so can't do
      -- anything (TODO)
      return ()

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

-- like the above, except both the function and the argument are reader
-- lenses
(^>>=^) :: forall (m :: * -> *) b r a. MonadReader r m =>
  Getting a r a -> Getting (a -> m b) r (a -> m b) -> m b
infixl 1 ^>>=^
la ^>>=^ lf = view lf >>= (view la >>=)

(^>>=) :: forall (m :: * -> *) a b r. MonadReader r m =>
  Getting a r a -> (a -> m b) -> m b
infixl 1 ^>>=
la ^>>= f = view la >>= f

(.>>=^) :: forall a (m :: * -> *) b r s.
  (MonadReader r m, MonadState s m) =>
  Getting a s a -> Getting (a -> m b) r (a -> m b) -> m b
infixl 1 .>>=^
la .>>=^ lf = view lf >>= (use la >>=)

-- apply a monadic function that's wrapped in the same monad to a pure value
infixl 1 >>=$
(>>=$) :: Monad m => m (a -> m b) -> a -> m b
mmf >>=$ x = mmf >>= ($ x)
