{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Juno.Runtime.Types
  ( Raft
  , RaftSpec(..)
  , readLogEntry, writeLogEntry, readTermNumber, writeTermNumber
  , readVotedFor, writeVotedFor, applyLogEntry, serializeRPC
  , deserializeRPC, sendMessage, getMessage, debugPrint
  , random, enqueue, dequeue, enqueueLater, killEnqueued
  , NodeID(..)
  , CommandEntry(..)
  , CommandResult(..)
  , Term(..), startTerm
  , LogIndex(..), startIndex
  , RequestId(..), startRequestId, toRequestId
  , Config(..), otherNodes, nodeId, electionTimeoutRange, heartbeatTimeout
  , enableDebug, publicKeys, clientPublicKeys, privateKey, clientTimeoutLimit
  , Role(..)
  , RaftEnv(..), cfg, quorumSize, rs
  , LogEntry(..)
  , RaftState(..), role, term, votedFor, lazyVote, currentLeader, ignoreLeader
  , logEntries, commitIndex, commitProof, lastApplied, timerThread, replayMap
  , cYesVotes, cPotentialVotes, lNextIndex, lMatchIndex, lConvinced
  , numTimeouts, pendingRequests, currentRequestId
  , initialRaftState
  -- * RPC
  , AppendEntries(..)
  , AppendEntriesResponse(..)
  , RequestVote(..)
  , RequestVoteResponse(..)
  , Command(..)
  , CommandResponse(..)
  , Revolution(..)
  , RPC(..)
  , Event(..)
  , HasSig(..)
  , signRPC
  , verifyRPC
  ) where

import Control.Concurrent (ThreadId)
import Control.Lens hiding (Index)
import Control.Monad.RWS (RWST)
import Codec.Crypto.RSA
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Serialize
import Data.Word (Word64)

import GHC.Int (Int64)
import GHC.Generics

import System.Random (Random)

-- | replaced et with CommandEntry
newtype CommandEntry = CommandEntry { unCommandEntry :: ByteString }
  deriving (Show, Eq, Ord, Generic, Serialize)

-- | replaced rt with CommandResp
newtype CommandResult = CommandResult { unCommandResult :: ByteString }
  deriving (Show, Eq, Ord, Generic, Serialize)

-- | replaced nt with NodeID
data NodeID = NodeID { _host :: String, _port :: Word64 }
  deriving (Eq,Ord,Read,Show,Generic)

instance Serialize NodeID

newtype Term = Term Int
  deriving (Show, Read, Eq, Enum, Num, Ord, Generic, Serialize)

startTerm :: Term
startTerm = Term (-1)

newtype LogIndex = LogIndex Int
  deriving (Show, Read, Eq, Ord, Enum, Num, Real, Integral, Generic, Serialize)

startIndex :: LogIndex
startIndex = LogIndex (-1)

newtype RequestId = RequestId Int64
  deriving (Show, Read, Eq, Ord, Enum, Num, Generic, Serialize)

startRequestId :: RequestId
startRequestId = RequestId 0

toRequestId :: Int64 -> RequestId
toRequestId a = RequestId a

data Config = Config
  { _otherNodes           :: Set NodeID -- Client,Server,Role,Sender,Simple
  , _nodeId               :: NodeID -- Client,Handler,Role,Sender,Simple,Util(debug)
  , _publicKeys           :: Map NodeID PublicKey -- Simple,Util(verifyRPCWithKey)
  , _clientPublicKeys     :: Map NodeID PublicKey -- Simple,Util(verifyRPCWithClientKey)
  , _privateKey           :: PrivateKey -- Sender,Simple,Util(signRPCWithKey)
  , _electionTimeoutRange :: (Int,Int) -- in microseconds  -- Timer
  , _heartbeatTimeout     :: Int       -- in microseconds  -- Timer
  , _enableDebug          :: Bool -- Simple
  , _clientTimeoutLimit   :: Int -- Client
  }
  deriving (Show, Generic)
makeLenses ''Config

class HasSig a where
    sig :: Lens' a LB.ByteString

data Command = Command
  { _cmdEntry     :: CommandEntry
  , _cmdClientId  :: NodeID
  , _cmdRequestId :: RequestId
  , _cmdSig       :: LB.ByteString
  }
  deriving (Show, Generic)

instance Serialize Command
instance HasSig Command where
    sig f s = fmap (\a -> s { _cmdSig = a }) (f (_cmdSig s))

data CommandResponse = CommandResponse
  { _cmdrResult    :: CommandResult
  , _cmdrLeaderId  :: NodeID
  , _cmdrNodeId    :: NodeID
  , _cmdrRequestId :: RequestId
  , _cmdrSig       :: LB.ByteString
  }
  deriving (Show, Generic)

instance Serialize CommandResponse
instance HasSig CommandResponse where
    sig f s = fmap (\a -> s { _cmdrSig = a }) (f (_cmdrSig s))

data LogEntry = LogEntry
  { _leTerm    :: Term
  , _leCommand :: Command
  , _leHash    :: B.ByteString
  }
  deriving (Show, Generic)

instance Serialize LogEntry

data AppendEntries = AppendEntries
  { _aeTerm        :: Term
  , _leaderId      :: NodeID
  , _prevLogIndex  :: LogIndex
  , _prevLogTerm   :: Term
  , _aeEntries     :: Seq LogEntry
  , _aeQuorumVotes :: Set RequestVoteResponse
  , _aeSig         :: LB.ByteString
  }
  deriving (Show, Generic)

instance Serialize AppendEntries
instance HasSig AppendEntries where
    sig f s = fmap (\a -> s { _aeSig = a }) (f (_aeSig s))

data AppendEntriesResponse = AppendEntriesResponse
  { _aerTerm      :: Term
  , _aerNodeId    :: NodeID
  , _aerSuccess   :: Bool
  , _aerConvinced :: Bool
  , _aerIndex     :: LogIndex
  , _aerHash      :: B.ByteString
  , _aerSig       :: LB.ByteString
  }
  deriving (Show, Generic, Eq, Ord)

instance Serialize AppendEntriesResponse
instance HasSig AppendEntriesResponse where
    sig f s = fmap (\a -> s { _aerSig = a }) (f (_aerSig s))

data RequestVote = RequestVote
  { _rvTerm        :: Term
  , _rvCandidateId :: NodeID
  , _lastLogIndex  :: LogIndex
  , _lastLogTerm   :: Term
  , _rvSig         :: LB.ByteString
  }
  deriving (Show, Generic)

instance Serialize RequestVote
instance HasSig RequestVote where
    sig f s = fmap (\a -> s { _rvSig = a }) (f (_rvSig s))

data RequestVoteResponse = RequestVoteResponse
  { _rvrTerm        :: Term
  , _rvrNodeId      :: NodeID
  , _voteGranted    :: Bool
  , _rvrCandidateId :: NodeID
  , _rvrSig         :: LB.ByteString
  }
  deriving (Show, Generic, Eq, Ord)

instance Serialize RequestVoteResponse
instance HasSig RequestVoteResponse where
    sig f s = fmap (\a -> s { _rvrSig = a }) (f (_rvrSig s))

data Revolution = Revolution
  { _revClientId  :: NodeID
  , _revLeaderId  :: NodeID
  , _revRequestId :: RequestId
  , _revSig       :: LB.ByteString
  }
  deriving (Show, Generic)

instance Serialize Revolution
instance HasSig Revolution where
    sig f s = fmap (\a -> s { _revSig = a }) (f (_revSig s))

data RPC = AE   AppendEntries
         | AER  AppendEntriesResponse
         | RV   RequestVote
         | RVR  RequestVoteResponse
         | CMD  Command
         | CMDR CommandResponse
         | REVOLUTION Revolution
         | DBG String
  deriving (Show, Generic)

instance Serialize RPC

signRPC :: (Serialize rpc, HasSig rpc) => PrivateKey -> rpc -> rpc
signRPC k rpc = set sig (sign k (encodeLazy (set sig LB.empty rpc))) rpc

verifyRPC :: (Serialize rpc, HasSig rpc) => PublicKey -> rpc -> Bool
verifyRPC k rpc = verify k (encodeLazy (set sig LB.empty rpc)) (view sig rpc)

data Event = ERPC RPC
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
    _readLogEntry     :: LogIndex -> m (Maybe CommandEntry) -- Simple [unused]

    -- ^ Function to write a log entry to persistent storage.
  , _writeLogEntry    :: LogIndex -> (Term,CommandEntry) -> m () -- Simple [unused]

    -- ^ Function to get the term number from persistent storage.
  , _readTermNumber   :: m Term -- Simple [unused]

    -- ^ Function to write the term number to persistent storage.
  , _writeTermNumber  :: Term -> m () -- Simple,Util(updateTerm[write only])

    -- ^ Function to read the node voted for from persistent storage.
  , _readVotedFor     :: m (Maybe NodeID) -- Simple [unused]

    -- ^ Function to write the node voted for to persistent storage.
  , _writeVotedFor    :: Maybe NodeID -> m () -- Simple,Role [write only]

    -- ^ Function to apply a log entry to the state machine.
  , _applyLogEntry    :: CommandEntry -> m CommandResult -- Simple,Handler

    -- ^ Function to serialize an RPC.
  , _serializeRPC     :: RPC -> ByteString -- Simple,Sender

    -- ^ Function to deserialize an RPC.
  , _deserializeRPC   :: ByteString -> Either String RPC -- Simple,Util(messageReceiver)

    -- ^ Function to send a message to a node.
  , _sendMessage      :: NodeID -> ByteString -> m () -- Simple,Sender

    -- ^ Function to get the next message.
  , _getMessage       :: m ByteString -- Simple,Util(messageReceiver)

    -- ^ Function to log a debug message (no newline).
  , _debugPrint       :: NodeID -> String -> m () -- Simple,Util(debug)

  , _random           :: forall a . Random a => (a, a) -> m a -- Simple,Util(randomRIO[timer])

  , _enqueue          :: Event -> m () -- Simple,Util(enqueueEvent)

  , _enqueueLater     :: Int -> Event -> m ThreadId -- Simple,Util(enqueueEventLater[timer])

  , _killEnqueued     :: ThreadId -> m () -- Simple,Timer

  , _dequeue          :: m Event -- Simple,Util(dequeueEvent)
  }
makeLenses (''RaftSpec)

data Role = Follower
          | Candidate
          | Leader
  deriving (Show, Generic, Eq)

data RaftState = RaftState
  { _role             :: Role -- Handler,Role,Util(debug)
  , _term             :: Term -- Handler,Role,Sender,Util(updateTerm)
  , _votedFor         :: Maybe NodeID -- Handler,Role
  , _lazyVote         :: Maybe (Term, NodeID) -- Handler
  , _currentLeader    :: Maybe NodeID -- Client,Handler,Role
  , _ignoreLeader     :: Bool -- Handler
  , _logEntries       :: Seq LogEntry -- Handler,Role,Sender
  , _commitIndex      :: LogIndex -- Handler
  , _lastApplied      :: LogIndex -- Handler
  , _commitProof      :: Map LogIndex (Set AppendEntriesResponse) -- Handler
  , _timerThread      :: Maybe ThreadId -- Timer
  , _replayMap        :: Map (NodeID, LB.ByteString) (Maybe CommandResult) -- Handler
  , _cYesVotes        :: Set RequestVoteResponse -- Handler,Role,Sender
  , _cPotentialVotes  :: Set NodeID -- Hander,Role,Sender
  , _lNextIndex       :: Map NodeID LogIndex -- Handler,Role,Sender
  , _lMatchIndex      :: Map NodeID LogIndex -- Role (never read?)
  , _lConvinced       :: Set NodeID -- Handler,Role,Sender

  -- used by clients
  , _pendingRequests  :: Map RequestId Command -- Client
  , _currentRequestId :: RequestId -- Client
  , _numTimeouts      :: Int -- Client
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
  Seq.empty  -- log
  startIndex -- commitIndex
  startIndex -- lastApplied
  Map.empty  -- commitProof
  Nothing    -- timerThread
  Map.empty  -- replayMap
  Set.empty  -- cYesVotes
  Set.empty  -- cPotentialVotes
  Map.empty  -- lNextIndex
  Map.empty  -- lMatchIndex
  Set.empty  -- lConvinced
  Map.empty  -- pendingRequests
  0          -- nextRequestId
  0          -- numTimeouts


type Raft m = RWST (RaftEnv m) () RaftState m

data RaftEnv m = RaftEnv
  { _cfg        :: Config
  , _quorumSize :: Int  -- Handler,Role
  , _rs         :: RaftSpec (Raft m)
  }
makeLenses ''RaftEnv
