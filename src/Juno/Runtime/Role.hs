
module Juno.Runtime.Role
  ( becomeFollower
  , becomeLeader
  , becomeCandidate
  , checkElection
  , setVotedFor
  ) where

import Control.Lens hiding (Index)
import Control.Monad

import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Juno.Runtime.Timer
import Juno.Runtime.Types
import Juno.Runtime.Sender
import Juno.Util.Util
import Juno.Util.Combinator

-- count the yes votes and become leader if you have reached a quorum
-- THREAD: SERVER MAIN
checkElection :: Monad m => Raft m ()
checkElection = do
  nyes <- Set.size <$> use cYesVotes
  qsize <- view quorumSize
  debug $ "yes votes: " ++ show nyes ++ " quorum size: " ++ show qsize
  when (nyes >= qsize) $ becomeLeader

-- THREAD: SERVER MAIN. updates state
setVotedFor :: Monad m => Maybe NodeID -> Raft m ()
setVotedFor mvote = do
  void $ rs.writeVotedFor ^$ mvote
  votedFor .= mvote

-- THREAD: unknown/unused. updates state
becomeFollower :: Monad m => Raft m ()
becomeFollower = do
  debug "becoming follower"
  role .= Follower
  resetElectionTimer

-- THREAD: SERVER MAIN. updates state
becomeCandidate :: Monad m => Raft m ()
becomeCandidate = do
  debug "becoming candidate"
  role .= Candidate
  use term >>= updateTerm . (+1)
  nid <- view (cfg.nodeId)
  setVotedFor (Just nid)
  ct <- use term
  selfVote <- signRPCWithKey $ RequestVoteResponse ct nid True nid B.empty
  cYesVotes .= Set.singleton selfVote
  (cPotentialVotes .=) =<< view (cfg.otherNodes)
  resetElectionTimer
  -- this is necessary for a single-node cluster, as we have already won the
  -- election in that case. otherwise we will wait for more votes to check again
  checkElection -- can possibly transition to leader
  r <- use role
  when (r == Candidate) sendAllRequestVotes -- THREAD: one-off, uses state but does not update

-- THREAD: SERVER MAIN. updates state
becomeLeader :: Monad m => Raft m ()
becomeLeader = do
  debug "becoming leader"
  role .= Leader
  (currentLeader .=) . Just =<< view (cfg.nodeId)
  ni <- Seq.length <$> use logEntries
  (lNextIndex  .=) =<< Map.fromSet (const $ LogIndex ni) <$> view (cfg.otherNodes)
  (lMatchIndex .=) =<< Map.fromSet (const startIndex) <$> view (cfg.otherNodes)
  lConvinced .= Set.empty
  sendAllAppendEntries
  resetHeartbeatTimer
