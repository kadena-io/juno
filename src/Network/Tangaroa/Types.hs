{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Tangaroa.Types
  ( Raft
  , RaftSpec(..)
  , LiftedRaftSpec(..)
  , readCfg, readLogEntry, writeLogEntry, readTermNumber, writeTermNumber
  , readVotedFor, writeVotedFor, applyLogEntry, openConnection, serializeRPC
  , deserializeRPC, sendMessage, getMessage, debugPrint
  , liftRaftSpec
  , Term, startTerm, succTerm
  , Index, startIndex
  , Config(..), nodeSet, nodeId, electionTimeoutRange, heartbeatTimeout
  , Role(..)
  , RaftEnv(..), cfg, conn, eventIn, eventOut, rs
  , RaftState(..), role, votedFor, logEntries, commitIndex, lastApplied, timerThread
  , cYesVotes, cNoVotes, cUndecided, lNextIndex, lMatchIndex
  , AppendEntries(..), aeTerm, leaderId, prevLogIndex, prevLogTerm
  , aeEntries, leaderCommit
  , AppendEntriesResponse(..), aerTerm, aerSuccess, aerNodeId, aerIndex
  , RequestVote(..), rvTerm, candidateId, lastLogIndex, lastLogTerm
  , RequestVoteResponse(..), rvrTerm, voterId, voteGranted
  , Command(..), entry
  , RPC(..)
  , term
  , Event(..)
  ) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.Chan.Unagi
import Control.Lens hiding (Index)
import Control.Monad.RWS
import Data.Binary
import Data.ByteString (ByteString)
import Data.Sequence (Seq)
import Data.Map (Map)
import Data.Set (Set)

import GHC.Generics

newtype Term = Term Int
  deriving (Show, Read, Eq, Ord, Generic)

startTerm :: Term
startTerm = Term (-1)

succTerm :: Term -> Term
succTerm (Term t) = Term (succ t)

type Index = Int

startIndex :: Index
startIndex = (-1)

data Config nt = Config
  { _nodeSet               :: Set nt
  , _nodeId                :: nt
  , _electionTimeoutRange  :: (Int,Int) -- in microseconds
  , _heartbeatTimeout      :: Int       -- in microseconds
  }
  deriving (Show, Generic)
makeLenses ''Config

data AppendEntries nt et = AppendEntries
  { _aeTerm       :: Term
  , _leaderId     :: nt
  , _prevLogIndex :: Index
  , _prevLogTerm  :: Term
  , _aeEntries    :: Seq (Term,et)
  , _leaderCommit :: Index
  }
  deriving (Show, Read, Generic)
makeLenses ''AppendEntries

data AppendEntriesResponse nt = AppendEntriesResponse
  { _aerTerm    :: Term
  , _aerNodeId  :: nt
  , _aerSuccess :: Bool
  , _aerIndex   :: Index
  }
  deriving (Show, Read, Generic)
makeLenses ''AppendEntriesResponse

data RequestVote nt = RequestVote
  { _rvTerm       :: Term
  , _candidateId  :: nt
  , _lastLogIndex :: Index
  , _lastLogTerm  :: Term
  }
  deriving (Show, Read, Generic)
makeLenses ''RequestVote

data RequestVoteResponse nt = RequestVoteResponse
  { _rvrTerm     :: Term
  , _voterId     :: nt
  , _voteGranted :: Bool
  }
  deriving (Show, Read, Generic)
makeLenses ''RequestVoteResponse

data Command et = Command
  { _entry :: et
  }
  deriving (Show, Read, Generic)
makeLenses ''Command

data RPC nt et rt = AE (AppendEntries nt et)
                  | AER (AppendEntriesResponse nt)
                  | RV (RequestVote nt)
                  | RVR (RequestVoteResponse nt)
                  | CMD (Command et)
                  | CMDR rt
                  | DBG String
  deriving (Show, Read, Generic)

-- | A structure containing all the implementation details for running
-- the raft protocol.
data RaftSpec nt et rt mt ht = RaftSpec
  {
    -- ^ Function to read configuration.
    __readCfg          :: IO (Config nt)

    -- ^ Function to get a log entry from persistent storage.
  , __readLogEntry     :: Index -> IO et

    -- ^ Function to write a log entry to persistent storage.
  , __writeLogEntry    :: Index -> (Term,et) -> IO ()

    -- ^ Function to get the term number from persistent storage.
  , __readTermNumber   :: IO Term

    -- ^ Function to write the term number to persistent storage.
  , __writeTermNumber  :: Term -> IO ()

    -- ^ Function to read the node voted for from persistent storage.
  , __readVotedFor     :: IO (Maybe nt)

    -- ^ Function to write the node voted for to persistent storage.
  , __writeVotedFor    :: Maybe nt -> IO ()

    -- ^ Function to apply a log entry to the state machine.
  , __applyLogEntry    :: et -> IO rt

    -- ^ Function to open a connection handle.
  , __openConnection   :: nt -> IO ht

    -- ^ Function to serialize an RPC.
  , __serializeRPC     :: RPC nt et rt -> mt

    -- ^ Function to deserialize an RPC.
  , __deserializeRPC   :: mt -> Maybe (RPC nt et rt)

    -- ^ Function to send a message to a node.
  , __sendMessage      :: nt -> mt -> IO ()

    -- ^ Function to get the next message.
  , __getMessage       :: ht -> IO mt

    -- ^ Function to log a debug message (no newline).
  , __debugPrint       :: String -> IO ()
  }

data LeaderState nt = LeaderState
  {
  }
  deriving (Show, Generic)
makeLenses ''LeaderState

data Role = Follower
          | Candidate
          | Leader
  deriving (Show, Generic, Eq)

data Event mt = Message mt
              | Election String
              | Heartbeat String
  deriving (Show)

-- | A version of RaftSpec where all IO functions are lifted
-- into the Raft monad.
data LiftedRaftSpec nt et rt mt ht t = LiftedRaftSpec
  {
    -- ^ Function to read configuration.
    _readCfg          :: MonadTrans t => t IO (Config nt)

    -- ^ Function to get a log entry from persistent storage.
  , _readLogEntry     :: MonadTrans t => Index -> t IO et

    -- ^ Function to write a log entry to persistent storage.
  , _writeLogEntry    :: MonadTrans t => Index -> (Term,et) -> t IO ()

    -- ^ Function to get the term number from persistent storage.
  , _readTermNumber   :: MonadTrans t => t IO Term

    -- ^ Function to write the term number to persistent storage.
  , _writeTermNumber  :: MonadTrans t => Term -> t IO ()

    -- ^ Function to read the node voted for from persistent storage.
  , _readVotedFor     :: MonadTrans t => t IO (Maybe nt)

    -- ^ Function to write the node voted for to persistent storage.
  , _writeVotedFor    :: MonadTrans t => Maybe nt -> t IO ()

    -- ^ Function to apply a log entry to the state machine.
  , _applyLogEntry    :: MonadTrans t => et -> t IO rt

    -- ^ Function to open a connection handle.
  , _openConnection   :: MonadTrans t => nt -> t IO ht

    -- ^ Function to serialize an RPC.
  , _serializeRPC     :: RPC nt et rt -> mt

    -- ^ Function to deserialize an RPC.
  , _deserializeRPC   :: mt -> Maybe (RPC nt et rt)

    -- ^ Function to send a message to a node.
  , _sendMessage      :: MonadTrans t => nt -> mt -> t IO ()

    -- ^ Function to get the next message.
  , _getMessage       :: MonadTrans t => ht -> t IO mt

    -- ^ Function to log a debug message (no newline).
  , _debugPrint       :: String -> t IO ()
  }
makeLenses ''LiftedRaftSpec

liftRaftSpec :: MonadTrans t => RaftSpec nt et rt mt ht -> LiftedRaftSpec nt et rt mt ht t
liftRaftSpec RaftSpec{..} =
  LiftedRaftSpec
    { _readCfg         = lift __readCfg
    , _readLogEntry    = lift . __readLogEntry
    , _writeLogEntry   = \i et -> lift (__writeLogEntry i et)
    , _readTermNumber  = lift __readTermNumber
    , _writeTermNumber = lift . __writeTermNumber
    , _readVotedFor    = lift __readVotedFor
    , _writeVotedFor   = lift . __writeVotedFor
    , _applyLogEntry   = lift . __applyLogEntry
    , _openConnection  = lift . __openConnection
    , _serializeRPC    = __serializeRPC
    , _deserializeRPC  = __deserializeRPC
    , _sendMessage     = \n m -> lift (__sendMessage n m)
    , _getMessage      = lift . __getMessage
    , _debugPrint      = lift . __debugPrint
    }

data RaftState nt et = RaftState
  { _role        :: Role
  , _term        :: Term
  , _votedFor    :: Maybe nt
  , _logEntries  :: Seq (Term,et)
  , _commitIndex :: Index
  , _lastApplied :: Index
  , _timerThread :: Maybe ThreadId
  , _cYesVotes   :: Map nt ByteString
  , _cNoVotes    :: Set nt
  , _cUndecided  :: Set nt
  , _lNextIndex  :: Map nt Index
  , _lMatchIndex :: Map nt Index
  }
  deriving (Show)
makeLenses ''RaftState

data RaftEnv nt et rt mt ht = RaftEnv
  { _cfg      :: Config nt
  , _conn     :: ht
  , _eventIn  :: InChan (Event mt)
  , _eventOut :: OutChan (Event mt)
  , _rs       :: LiftedRaftSpec nt et rt mt ht (RWST (RaftEnv nt et rt mt ht) () (RaftState nt et))
  }
makeLenses ''RaftEnv

type Raft nt et rt mt ht a = RWST (RaftEnv nt et rt mt ht) () (RaftState nt et) IO a

instance Binary Term

instance (Binary nt, Binary et) => Binary (AppendEntries nt et)
instance Binary nt              => Binary (AppendEntriesResponse nt)
instance Binary nt              => Binary (RequestVote nt)
instance Binary nt              => Binary (RequestVoteResponse nt)
instance Binary et              => Binary (Command et)

instance (Binary nt, Binary et, Binary rt) => Binary (RPC nt et rt)
