{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Juno.Runtime.Protocol.Types
--
-- Holds the core Juno/Raft Types used to implement BFT Raft
-- the types here are internal to the protocol nodes, but for now they share
-- some types Runtime/Types.hs with Api/Types.hs.

module Juno.Types.State
  ( Raft
  , RaftSpec(..)
  , readLogEntry, writeLogEntry, readTermNumber, writeTermNumber
  , readVotedFor, writeVotedFor, applyLogEntry, sendMessage
  , sendMessages, getMessage, getMessages, getNewCommands, getNewEvidence
  , debugPrint, publishMetric, getTimestamp, random
  , enqueue, enqueueMultiple, dequeue, enqueueLater, killEnqueued
  -- for API <-> Juno communication
  , dequeueFromApi ,cmdStatusMap, updateCmdMap
  , RaftEnv(..), cfg, clusterSize, quorumSize, rs
  , RaftState(..), nodeRole, term, votedFor, lazyVote, currentLeader, ignoreLeader
  , logEntries, commitIndex, commitProof, lastApplied, timerThread, replayMap
  , cYesVotes, cPotentialVotes, lNextIndex, lMatchIndex, lConvinced
  , lastCommitTime, numTimeouts, pendingRequests, currentRequestId
  , timeSinceLastAER, lLastBatchUpdate
  , initialRaftState
  , Event(..)
  ) where

import Control.Concurrent (MVar, ThreadId)
import Control.Lens hiding (Index, (|>))
import Control.Monad.RWS (RWST)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.ByteString (ByteString)
import Data.Thyme.Clock
import Data.Thyme.Time.Core ()
import System.Random (Random)

import Juno.Types.Base
import Juno.Types.Config
import Juno.Types.Message
import Juno.Types.Command
import Juno.Types.Metric
import Juno.Types.Log

data Event = ERPC RPC
           | AERs AlotOfAERs
           | ElectionTimeout String
           | HeartbeatTimeout String
  deriving (Show)

-- | A structure containing all the implementation details for running
-- the raft protocol.
-- Types:
-- nt -- "node type", ie identifier (host/port, topic, subject)
-- et -- "entry type", serialized format for submissions into Raft
-- rt -- "return type", serialized format for "results" or "responses"
-- mt -- "message type", serialized format for sending over wire
data RaftSpec m = RaftSpec
  {
    -- ^ Function to get a log entry from persistent storage.
    _readLogEntry     :: LogIndex -> m (Maybe CommandEntry)

    -- ^ Function to write a log entry to persistent storage.
  , _writeLogEntry    :: LogIndex -> (Term,CommandEntry) -> m ()

    -- ^ Function to get the term number from persistent storage.
  , _readTermNumber   :: m Term

    -- ^ Function to write the term number to persistent storage.
  , _writeTermNumber  :: Term -> m ()

    -- ^ Function to read the node voted for from persistent storage.
  , _readVotedFor     :: m (Maybe NodeID)

    -- ^ Function to write the node voted for to persistent storage.
  , _writeVotedFor    :: Maybe NodeID -> m ()

    -- ^ Function to apply a log entry to the state machine.
  , _applyLogEntry    :: CommandEntry -> m CommandResult

    -- ^ Function to send a message to a node.
  , _sendMessage      :: NodeID -> ByteString -> m ()

    -- ^ Send more than one message at once
  , _sendMessages     :: [(NodeID,ByteString)] -> m ()

    -- ^ Function to get the next message.
  , _getMessage       :: m (ReceivedAt, SignedRPC)

    -- ^ Function to get the next N SignedRPCs not of type CMD, CMDB, or AER
  , _getMessages   :: Int -> m [(ReceivedAt, SignedRPC)]

    -- ^ Function to get the next N SignedRPCs of type CMD or CMDB
  , _getNewCommands   :: Int -> m [(ReceivedAt, SignedRPC)]

    -- ^ Function to get the next N SignedRPCs of type AER
  , _getNewEvidence   :: Int -> m [(ReceivedAt, SignedRPC)]

    -- ^ Function to log a debug message (no newline).
  , _debugPrint       :: NodeID -> String -> m ()

  , _publishMetric    :: Metric -> m ()

  , _getTimestamp     :: m UTCTime

  , _random           :: forall a . Random a => (a, a) -> m a

  , _enqueue          :: Event -> m ()

  , _enqueueMultiple  :: [Event] -> m ()

  , _enqueueLater     :: Int -> Event -> m ThreadId

  , _killEnqueued     :: ThreadId -> m ()

  , _dequeue          :: m Event

  -- ^ How the API communicates with Raft, later could be redis w/e, etc.
  , _updateCmdMap     :: MVar CommandMap -> RequestId -> CommandStatus -> m ()

  -- ^ Same mvar map as _updateCmdMVarMap needs to run in Raft m
  , _cmdStatusMap     :: CommandMVarMap

  , _dequeueFromApi   :: m (RequestId, [CommandEntry])
  }
makeLenses (''RaftSpec)

data RaftState = RaftState
  { _nodeRole             :: Role
  , _term             :: Term
  , _votedFor         :: Maybe NodeID
  , _lazyVote         :: Maybe (Term, NodeID, LogIndex)
  , _currentLeader    :: Maybe NodeID
  , _ignoreLeader     :: Bool
  , _logEntries       :: Log LogEntry
  , _commitIndex      :: LogIndex
  , _lastApplied      :: LogIndex
  , _commitProof      :: Map NodeID AppendEntriesResponse
  , _timerThread      :: Maybe ThreadId
  , _timeSinceLastAER :: Int
  , _replayMap        :: Map (NodeID, Signature) (Maybe CommandResult)
  , _cYesVotes        :: Set RequestVoteResponse
  , _cPotentialVotes  :: Set NodeID
  , _lNextIndex       :: Map NodeID LogIndex
  , _lMatchIndex      :: Map NodeID LogIndex
  , _lConvinced       :: Set NodeID
  , _lLastBatchUpdate :: (UTCTime, Maybe ByteString)
  -- used for metrics
  , _lastCommitTime   :: Maybe UTCTime
  -- used by clients
  , _pendingRequests  :: Map RequestId Command
  , _currentRequestId :: RequestId
  , _numTimeouts      :: Int
  }
makeLenses ''RaftState

initialRaftState :: RaftState
initialRaftState = RaftState
  Follower   -- role
  startTerm  -- term
  Nothing    -- votedFor
  Nothing    -- lazyVote
  Nothing    -- currentLeader
  False      -- ignoreLeader
  mempty     -- log
  startIndex -- commitIndex
  startIndex -- lastApplied
  Map.empty  -- commitProof
  Nothing    -- timerThread
  0          -- timeSinceLastAER
  Map.empty  -- replayMap
  Set.empty  -- cYesVotes
  Set.empty  -- cPotentialVotes
  Map.empty  -- lNextIndex
  Map.empty  -- lMatchIndex
  Set.empty  -- lConvinced
  (minBound, Nothing)   -- lLastBatchUpdate (when we start up, we want batching to fire immediately)
  Nothing    -- lastCommitTime
  Map.empty  -- pendingRequests
  0          -- nextRequestId
  0          -- numTimeouts

type Raft m = RWST (RaftEnv m) () RaftState m

data RaftEnv m = RaftEnv
  { _cfg         :: Config
  , _clusterSize :: Int
  , _quorumSize  :: Int
  , _rs          :: RaftSpec (Raft m)
  }
makeLenses ''RaftEnv
