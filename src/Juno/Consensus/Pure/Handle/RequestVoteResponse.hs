{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Juno.Consensus.Pure.Handle.RequestVoteResponse
    (handle)
where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer.Strict
import Data.Map as Map
import Data.Set as Set

import Juno.Consensus.Pure.Types
import Juno.Runtime.Sender (sendAllAppendEntries)
import Juno.Runtime.Timer (resetHeartbeatTimer, resetElectionTimerLeader,
                           resetElectionTimer)
import Juno.Util.Util
import Juno.Types.Log
import qualified Juno.Types as JT

data RequestVoteResponseEnv = RequestVoteResponseEnv {
      _nodeRole :: Role
    , _term :: Term
    , _lastLogIndex :: LogIndex
    , _cYesVotes :: Set.Set RequestVoteResponse
    , _quorumSize :: Int
}
makeLenses ''RequestVoteResponseEnv

data RequestVoteResponseOut =
    BecomeLeader { _newYesVotes :: Set.Set RequestVoteResponse } |
    UpdateYesVotes { _newYesVotes :: Set.Set RequestVoteResponse } |
    DeletePotentialVote { _voteNodeId :: NodeID } |
    RevertToFollower |
    NoAction

handleRequestVoteResponse :: (MonadReader RequestVoteResponseEnv m, MonadWriter [String] m) =>
                             RequestVoteResponse -> m RequestVoteResponseOut
handleRequestVoteResponse rvr@RequestVoteResponse{..} = do
  tell ["got a requestVoteResponse RPC for " ++ show _rvrTerm ++ ": " ++ show _voteGranted]
  r <- view nodeRole
  ct <- view term
  curLog <- view lastLogIndex
  if r == Candidate && ct == _rvrTerm
  then
    if _voteGranted
    then Set.insert rvr <$> view cYesVotes >>= checkElection
    else
        return $ DeletePotentialVote _rvrNodeId
  else if ct > _rvrTerm && _rvrCurLogIndex > curLog && r == Candidate
       -- We are a runaway candidate is a bad state and need to revert to our last know good state
       -- A Candidate which reverts to it's last good state (specifically the Term of its last LogEntry)
       -- is not distinguishable from an out of date follower and an out of date follower is already
       -- handled by raft. Thus this operation is safe.
       then do
            tell ["Log is too out of date to ever become leader, revert to last good state: "
                  ++ show (ct,curLog) ++ " vs " ++ show (_rvrTerm, _rvrCurLogIndex)]
            return RevertToFollower
       else tell ["Taking no action on RVR"] >> return NoAction



-- count the yes votes and become leader if you have reached a quorum
checkElection :: (MonadReader RequestVoteResponseEnv m, MonadWriter [String] m) =>
                 Set.Set RequestVoteResponse -> m RequestVoteResponseOut
checkElection votes = do
  nyes <- return $ Set.size votes
  qsize <- view quorumSize
  tell ["yes votes: " ++ show nyes ++ " quorum size: " ++ show qsize]
  if nyes >= qsize
  then tell ["becoming leader"] >> return (BecomeLeader votes)
  else return $ UpdateYesVotes votes


handle :: Monad m => RequestVoteResponse -> JT.Raft m ()
handle m = do
  r <- ask
  s <- get
  es <- use JT.logEntries
  (o,l) <- runReaderT (runWriterT (handleRequestVoteResponse m))
           (RequestVoteResponseEnv
            (JT._nodeRole s)
            (JT._term s)
            (maxIndex es)
            (JT._cYesVotes s)
            (JT._quorumSize r))
  mapM_ debug l
  case o of
    BecomeLeader vs -> do
             JT.cYesVotes .= vs
             becomeLeader
    UpdateYesVotes vs -> JT.cYesVotes .= vs
    DeletePotentialVote n -> JT.cPotentialVotes %= Set.delete n
    NoAction -> return ()
    RevertToFollower -> revertToLastQuorumState


-- THREAD: SERVER MAIN. updates state
becomeLeader :: Monad m => JT.Raft m ()
becomeLeader = do
  setRole Leader
  setCurrentLeader . Just =<< view (JT.cfg.JT.nodeId)
  ni <- entryCount <$> use JT.logEntries
  setLNextIndex =<< Map.fromSet (const ni) <$> view (JT.cfg.JT.otherNodes)
  (JT.lMatchIndex .=) =<< Map.fromSet (const startIndex) <$> view (JT.cfg.JT.otherNodes)
  JT.lConvinced .= Set.empty
  sendAllAppendEntries
  resetHeartbeatTimer
  resetElectionTimerLeader

revertToLastQuorumState :: Monad m => JT.Raft m ()
revertToLastQuorumState = do
  es <- use JT.logEntries
  setRole Follower
  -- We don't persist this info and don't want to trust the RVR send's
  -- word so we set it to nothing an await an AE from some Leader of a higher term, then validate the votes
  setCurrentLeader Nothing
  JT.ignoreLeader .= False
  setTerm (lastLogTerm es)
  JT.votedFor .= Nothing
  JT.cYesVotes .= Set.empty
  JT.cPotentialVotes .= Set.empty
  resetElectionTimer
