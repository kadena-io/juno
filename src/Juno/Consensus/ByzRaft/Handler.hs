{-# LANGUAGE RecordWildCards #-}

module Juno.Consensus.ByzRaft.Handler
  ( handleEvents
  ) where

import Control.Lens
import Control.Monad hiding (mapM)
import Control.Monad.Loops
import Codec.Digest.SHA
import Data.Serialize
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Foldable (all, traverse_)
import Data.Traversable (mapM)
import Prelude hiding (mapM, all)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Juno.Runtime.Types
import Juno.Runtime.Sender
import Juno.Util.Util
import Juno.Runtime.Role
import Juno.Runtime.Timer


-- THREAD: SERVER MAIN
handleEvents :: Monad m => Raft m ()
handleEvents = forever $ do
  e <- dequeueEvent
  case e of
    ERPC rpc           -> handleRPC rpc
    ElectionTimeout s  -> handleElectionTimeout s
    HeartbeatTimeout s -> handleHeartbeatTimeout s

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb ma = do
  b <- mb
  when b ma

handleRPC :: Monad m => RPC -> Raft m ()
handleRPC rpc = case rpc of
  AE ae          -> whenM (verifyRPCWithKey rpc) $ handleAppendEntries ae
  AER aer        -> whenM (verifyRPCWithKey rpc) $ handleAppendEntriesResponse aer
  RV rv          -> whenM (verifyRPCWithKey rpc) $ handleRequestVote rv
  RVR rvr        -> whenM (verifyRPCWithKey rpc) $ handleRequestVoteResponse rvr
  CMD cmd        -> whenM (verifyRPCWithClientKey rpc) $ handleCommand cmd
  CMDR _         -> whenM (verifyRPCWithKey rpc) $ debug "got a command response RPC"
  DBG s          -> debug $ "got a debug RPC: " ++ s
  REVOLUTION rev -> whenM (verifyRPCWithClientKey rpc) $ handleRevolution rev

-- THREAD: SERVER MAIN. updates state
handleElectionTimeout :: Monad m => String -> Raft m ()
handleElectionTimeout s = do
  debug $ "election timeout: " ++ s
  r <- use role
  when (r /= Leader) $ do
    lv <- use lazyVote
    case lv of
      Just (t, c) -> do
        updateTerm t
        setVotedFor (Just c)
        lazyVote .= Nothing
        ignoreLeader .= False
        currentLeader .= Nothing
        sendRequestVoteResponse c True
        resetElectionTimer
      Nothing -> becomeCandidate

-- THREAD: SERVER MAIN. updates state
handleHeartbeatTimeout :: Monad m => String -> Raft m ()
handleHeartbeatTimeout s = do
  debug $ "heartbeat timeout: " ++ s
  r <- use role
  when (r == Leader) $ do
    sendAllAppendEntries
    resetHeartbeatTimer

-- THREAD: SERVER MAIN. updates state
checkForNewLeader :: Monad m => AppendEntries -> Raft m ()
checkForNewLeader AppendEntries{..} = do
  ct <- use term
  cl <- use currentLeader
  if (_aeTerm == ct && cl == Just _leaderId) ||
      _aeTerm < ct ||
      Set.size _aeQuorumVotes == 0
    then return ()
    else do
      debug $ "New leader identified: " ++ show _leaderId
      votesValid <- confirmElection _leaderId _aeTerm _aeQuorumVotes
      debug $ "New leader votes are valid: " ++ show votesValid
      when votesValid $ do
        updateTerm _aeTerm
        ignoreLeader .= False
        currentLeader .= Just _leaderId
        role .= Follower

confirmElection :: Monad m => NodeID -> Term -> Set RequestVoteResponse -> Raft m Bool
confirmElection l t votes = do
  debug "confirming election of a new leader"
  qsize <- view quorumSize
  if Set.size votes >= qsize
    then allM (validateVote l t) (Set.toList votes)
    else return False

validateVote :: Monad m => NodeID -> Term -> RequestVoteResponse -> Raft m Bool
validateVote l t vote@RequestVoteResponse{..} = do
  sigOkay <- verifyRPCWithKey (RVR vote)
  return (sigOkay && _rvrCandidateId == l && _rvrTerm == t)

-- THREAD: SERVER MAIN. updates state
handleAppendEntries :: Monad m => AppendEntries -> Raft m ()
handleAppendEntries ae@AppendEntries{..} = do
  debug $ "got an appendEntries RPC: prev log entry: Index " ++ show _prevLogIndex ++ " " ++ show _prevLogTerm
  checkForNewLeader ae
  cl <- use currentLeader
  ig <- use ignoreLeader
  ct <- use term
  case cl of
    Just l | not ig && l == _leaderId && _aeTerm == ct -> do
      resetElectionTimer
      lazyVote .= Nothing
      plmatch <- prevLogEntryMatches _prevLogIndex _prevLogTerm
      if not plmatch
        then sendAppendEntriesResponse _leaderId False True
        else do
          appendLogEntries _prevLogIndex _aeEntries
          doCommit
          {-|
          if (not (Seq.null _aeEntries))
            -- only broadcast when there are new entries
            -- this has the downside that recovering nodes won't update
            -- their commit index until new entries come along
            -- not sure if this is okay or not
            -- committed entries by definition have already been externalized
            -- so if a particular node missed it, there were already 2f+1 nodes
            -- that didn't
            then sendAllAppendEntriesResponse
            else sendAppendEntriesResponse _leaderId True True
          --}
      sendAllAppendEntriesResponse
    _ | not ig && _aeTerm >= ct -> do
      debug "sending unconvinced response"
      sendAppendEntriesResponse _leaderId False False
    _ -> return ()

mergeCommitProof :: Monad m => AppendEntriesResponse -> Raft m ()
mergeCommitProof aer@AppendEntriesResponse{..} = do
  ci <- use commitIndex
  debug $ "merging commit proof for index: " ++ show _aerIndex
  when (_aerIndex > ci) $
    commitProof.at _aerIndex %= maybe (Just (Set.singleton aer)) (Just . Set.insert aer)

prevLogEntryMatches :: Monad m => LogIndex -> Term -> Raft m Bool
prevLogEntryMatches pli plt = do
  es <- use logEntries
  case seqIndex es $ fromIntegral pli of
    -- if we don't have the entry, only return true if pli is startIndex
    Nothing    -> return (pli == startIndex)
    -- if we do have the entry, return true if the terms match
    Just LogEntry{..} -> return (_leTerm == plt)

-- THREAD: SERVER MAIN. updates state
appendLogEntries :: Monad m => LogIndex -> Seq (LogEntry) -> Raft m ()
appendLogEntries pli es = do
  logEntries %= (Seq.>< es) . Seq.take (fromIntegral pli + 1)
  debug $ "replaying LogEntry: " ++ show es
  traverse_ (\LogEntry{_leCommand = Command{..}} -> replayMap %= Map.insert (_cmdClientId, _cmdSig) Nothing) es
  updateLogHashesFromIndex (pli + 1)

hashLogEntry :: Maybe (LogEntry) -> LogEntry -> LogEntry
hashLogEntry (Just LogEntry{ _leHash = prevHash}) le =
  le { _leHash = hash SHA256 (encode (le { _leHash = prevHash }))}
hashLogEntry Nothing le =
  le { _leHash = hash SHA256 (encode (le { _leHash = B.empty }))}

-- THREAD: SERVER MAIN. updates state
updateLogHashesFromIndex :: Monad m => LogIndex -> Raft m ()
updateLogHashesFromIndex i = do
  es <- use logEntries
  case seqIndex es $ fromIntegral i of
    Just _  -> do
      logEntries %= Seq.adjust (hashLogEntry (seqIndex es (fromIntegral i - 1))) (fromIntegral i)
      updateLogHashesFromIndex (i + 1)
    Nothing -> return ()

addLogEntryAndHash :: LogEntry -> Seq (LogEntry) -> Seq (LogEntry)
addLogEntryAndHash le es =
  case Seq.viewr es of
    _ Seq.:> ple -> es Seq.|> hashLogEntry (Just ple) le
    Seq.EmptyR   -> Seq.singleton (hashLogEntry Nothing le)

-- THREAD: SERVER MAIN. updates state
handleAppendEntriesResponse :: Monad m => AppendEntriesResponse -> Raft m ()
handleAppendEntriesResponse aer@AppendEntriesResponse{..} = do
  debug "got an appendEntriesResponse RPC"
  mergeCommitProof aer
  doCommit
  r <- use role
  ct <- use term
  when (r == Leader) $ do
    when (not _aerConvinced && _aerTerm <= ct) $ -- implies not _aerSuccess
      lConvinced %= Set.delete _aerNodeId
    when (_aerTerm == ct) $ do
      when (_aerConvinced && not _aerSuccess) $
        lNextIndex %= Map.adjust (subtract 1) _aerNodeId
      when (_aerConvinced && _aerSuccess) $ do
        lNextIndex .at _aerNodeId .= Just (_aerIndex + 1)
        lConvinced %= Set.insert _aerNodeId
    when (not _aerConvinced || not _aerSuccess) $
      sendAppendEntries _aerNodeId

applyCommand :: Monad m => Command -> Raft m (NodeID, CommandResponse)
applyCommand cmd@Command{..} = do
  apply <- view (rs.applyLogEntry)
  result <- apply _cmdEntry
  replayMap %= Map.insert (_cmdClientId, _cmdSig) (Just result)
  ((,) _cmdClientId) <$> makeCommandResponse cmd result

makeCommandResponse :: Monad m => Command -> CommandResult -> Raft m CommandResponse
makeCommandResponse Command{..} result = do
  nid <- view (cfg.nodeId)
  mlid <- use currentLeader
  return $ CommandResponse
             result
             (maybe nid id mlid)
             nid
             _cmdRequestId
             LB.empty

-- THREAD: SERVER MAIN.
doCommit :: Monad m => Raft m ()
doCommit = do
  commitUpdate <- updateCommitIndex
  when commitUpdate applyLogEntries

-- apply the un-applied log entries up through commitIndex
-- and send results to the client if you are the leader
-- TODO: have this done on a separate thread via event passing
-- THREAD: SERVER MAIN. updates state
applyLogEntries :: Monad m => Raft m ()
applyLogEntries = do
  la <- use lastApplied
  ci <- use commitIndex
  le <- use logEntries
  let leToApply = Seq.drop (fromIntegral $ la + 1) . Seq.take (fromIntegral $ ci + 1) $ le
  results <- mapM (applyCommand . _leCommand) leToApply
  r <- use role
  when (r == Leader) $ sendResults results
  lastApplied .= ci


-- checks to see what the largest N where a quorum of nodes
-- has sent us proof of a commit up to that index
-- THREAD: SERVER MAIN. updates state
updateCommitIndex :: Monad m => Raft m Bool
updateCommitIndex = do
  ci <- use commitIndex
  proof <- use commitProof
  qsize <- view quorumSize
  es <- use logEntries

  -- get all indices in the log past commitIndex
  -- TODO: look into the overloading of LogIndex w.r.t. Seq Length/entry location
  let inds = [(ci + 1)..(fromIntegral $ Seq.length es - 1)]

  -- get the prefix of these indices where a quorum of nodes have
  -- provided proof of having replicated that entry
  let qcinds = takeWhile (\i -> (not . Map.null) (Map.filterWithKey (\k s -> k >= i && Set.size s + 1 >= qsize) proof)) inds

  case qcinds of
    [] -> return False
    _  -> do
      let qci = last qcinds
      case Map.lookup qci proof of
        Just s -> do
          let lhash = _leHash (Seq.index es $ fromIntegral qci)
          valid <- checkCommitProof qci lhash s
          if valid
            then do
              commitIndex .= qci
              commitProof %= Map.filterWithKey (\k _ -> k >= qci)
              debug $ "commit index is now: " ++ show qci
              return True
            else
              return False
        Nothing -> return False

checkCommitProof :: Monad m => LogIndex -> B.ByteString -> Set AppendEntriesResponse -> Raft m Bool
checkCommitProof ci lhash aers = do
  sigsOkay <- allM (verifyRPCWithKey . AER) (Set.toList aers)
  return $ sigsOkay && all (\AppendEntriesResponse{..} -> _aerHash == lhash && _aerIndex == ci) aers

-- THREAD: SERVER MAIN. updates state and launches threads.
handleRequestVote :: Monad m => RequestVote -> Raft m ()
handleRequestVote RequestVote{..} = do
  debug $ "got a requestVote RPC for " ++ show _rvTerm
  mvote <- use votedFor
  es <- use logEntries
  ct <- use term
  cl <- use currentLeader
  ig <- use ignoreLeader
  case mvote of
    _      | ig && cl == Just _rvCandidateId -> return ()
      -- don't respond to a candidate if they were leader and a client
      -- asked us to ignore them

    _      | _rvTerm < ct -> do
      -- this is an old candidate
      debug "this is for an old term"
      sendRequestVoteResponse _rvCandidateId False

    Just c | c == _rvCandidateId && _rvTerm == ct -> do
      -- already voted for this candidate in this term
      debug "already voted for this candidate"
      sendRequestVoteResponse _rvCandidateId True

    Just _ | _rvTerm == ct -> do
      -- already voted for a different candidate in this term
      debug "already voted for a different candidate"
      sendRequestVoteResponse _rvCandidateId False

    _ -> if (_lastLogTerm, _lastLogIndex) >= let (llt, lli, _) = lastLogInfo es in (llt, lli)
      -- we have no recorded vote, or this request is for a higher term
      -- (we don't externalize votes without updating our own term, so we
      -- haven't voted in the higher term before)
      -- lazily vote for the candidate if its log is at least as
      -- up to date as ours, use the Ord instance of (Term, Index) to prefer
      -- higher terms, and then higher last indices for equal terms
      then do
        lv <- use lazyVote
        case lv of
          Just (t, _) | t >= _rvTerm ->
            debug "would vote lazily, but already voted lazily for candidate in same or higher term"
          Just _ -> do
            debug "replacing lazy vote"
            lazyVote .= Just (_rvTerm, _rvCandidateId)
          Nothing -> do
            debug "haven't voted, (lazily) voting for this candidate"
            lazyVote .= Just (_rvTerm, _rvCandidateId)
      else do
        debug "haven't voted, but my log is better than this candidate's"
        sendRequestVoteResponse _rvCandidateId False

-- THREAD: SERVER MAIN. updates state
handleRequestVoteResponse :: Monad m => RequestVoteResponse -> Raft m ()
handleRequestVoteResponse rvr@RequestVoteResponse{..} = do
  debug $ "got a requestVoteResponse RPC for " ++ show _rvrTerm ++ ": " ++ show _voteGranted
  r <- use role
  ct <- use term
  when (r == Candidate && ct == _rvrTerm) $
    if _voteGranted
      then do
        cYesVotes %= Set.insert rvr
        checkElection
      else
        cPotentialVotes %= Set.delete _rvrNodeId

-- THREAD: SERVER MAIN. updates state
handleCommand :: Monad m => Command -> Raft m ()
handleCommand cmd@Command{..} = do
  debug $ "got a command RPC"
  r <- use role
  ct <- use term
  mlid <- use currentLeader
  replays <- use replayMap
  case (Map.lookup (_cmdClientId, _cmdSig) replays, r, mlid) of
    (Just (Just result), _, _) -> do
      cmdr <- makeCommandResponse cmd result
      sendSignedRPC _cmdClientId $ CMDR cmdr
      -- we have already committed this request, so send the result to the client
    (Just Nothing, _, _) ->
      -- we have already seen this request, but have not yet committed it
      -- nothing to do
      return ()
    (_, Leader, _) -> do
      -- we're the leader, so append this to our log with the current term
      -- and propagate it to replicas
      logEntries %= addLogEntryAndHash (LogEntry ct cmd B.empty)
      replayMap %= Map.insert (_cmdClientId, _cmdSig) Nothing
      sendAllAppendEntries
      sendAllAppendEntriesResponse
      doCommit
    (_, _, Just lid) ->
      -- we're not the leader, but we know who the leader is, so forward this
      -- command (don't sign it ourselves, as it comes from the client)
      sendRPC lid $ CMD cmd -- THREAD: one-off, no state
    (_, _, Nothing) ->
      -- we're not the leader, and we don't know who the leader is, so can't do
      -- anything
      return ()

-- THREAD: SERVER MAIN. updates state
handleRevolution :: Monad m => Revolution -> Raft m ()
handleRevolution Revolution{..} = do
  cl <- use currentLeader
  whenM (Map.notMember (_revClientId, _revSig) <$> use replayMap) $
    case cl of
      Just l | l == _revLeaderId -> do
        replayMap %= Map.insert (_revClientId, _revSig) Nothing
        -- clear our lazy vote if it was for this leader
        lv <- use lazyVote
        case lv of
          Just (_, lvid) | lvid == _revLeaderId -> lazyVote .= Nothing
          _ -> return ()
        ignoreLeader .= True
      _ -> return ()
