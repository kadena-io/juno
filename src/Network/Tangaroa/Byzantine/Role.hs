module Network.Tangaroa.Byzantine.Role
  ( becomeFollower
  , becomeLeader
  , becomeCandidate
  , checkElection
  , setVotedFor
  ) where

import Control.Lens hiding (Index)
import Control.Monad
import Data.Functor
import Data.Binary
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Network.Tangaroa.Byzantine.Timer
import Network.Tangaroa.Byzantine.Types
import Network.Tangaroa.Byzantine.Util
import Network.Tangaroa.Byzantine.Sender
import Network.Tangaroa.Combinator

-- count the yes votes and become leader if you have reached a quorum
checkElection :: (Binary nt, Binary et, Binary rt, Ord nt) => Raft nt et rt mt ()
checkElection = do
  nyes <- Set.size <$> use cYesVotes
  qsize <- view quorumSize
  debug $ "yes votes: " ++ show nyes ++ " quorum size: " ++ show qsize
  when (nyes >= qsize) $ becomeLeader

setVotedFor :: Maybe nt -> Raft nt et rt mt ()
setVotedFor mvote = do
  rs.writeVotedFor ^$ mvote
  votedFor .= mvote

becomeFollower :: Raft nt et rt mt ()
becomeFollower = do
  debug "becoming follower"
  role .= Follower
  resetElectionTimer

becomeCandidate :: (Binary nt, Binary et, Binary rt, Ord nt) => Raft nt et rt mt ()
becomeCandidate = do
  debug "becoming candidate"
  role .= Candidate
  use term >>= updateTerm . (+ 1)
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
  when (r == Candidate) $ fork_ sendAllRequestVotes

becomeLeader :: (Binary nt, Binary et, Binary rt, Ord nt) => Raft nt et rt mt ()
becomeLeader = do
  debug "becoming leader"
  role .= Leader
  (currentLeader .=) . Just =<< view (cfg.nodeId)
  ni <- Seq.length <$> use logEntries
  (lNextIndex  .=) =<< Map.fromSet (const ni)         <$> view (cfg.otherNodes)
  (lMatchIndex .=) =<< Map.fromSet (const startIndex) <$> view (cfg.otherNodes)
  lConvinced .= Set.empty
  fork_ sendAllAppendEntries
  resetHeartbeatTimer
