{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Tangaroa.Types
  ( Raft
  , RaftSpec(..)
  , LiftedRaftSpec(..)
  , readCfg, readLogEntry, writeLogEntry, readTermNumber, writeTermNumber
  , readVotedFor, writeVotedFor, commitLogEntry, openConnection, serializeRPC
  , deserializeRPC , sendMessage , getMessage
  , liftRaftSpec
  , Term, startTerm, succTerm
  , Index, startIndex, succIndex
  , Config(..), nodeSet, nodeId, electionTimeoutRange, heartbeatTimeout
  , CandidateState(..), votes
  , LeaderState(..), nextIndex, matchIndex
  , Role(..)
  , RaftEnv(..), cfg, conn, eventIn, eventOut, rs
  , RaftState(..), role, commitIndex, lastApplied, timerThread
  , AppendEntries(..), aeTerm, leaderId, prevLogIndex, prevLogTerm, entries, leaderCommit
  , AppendEntriesResponse(..), aerTerm, success
  , RequestVote(..), rvTerm, candidateId, lastLogIndex, lastLogTerm
  , RequestVoteResponse(..), rvrTerm, voteGranted
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
import Data.Map (Map)
import Data.Set (Set)
--import Data.Word
import GHC.Generics

newtype Term = Term Word64
  deriving (Show, Read, Eq, Ord, Generic)

startTerm :: Term
startTerm = Term 0

succTerm :: Term -> Term
succTerm (Term t) = Term (succ t)

newtype Index = Index Word64
  deriving (Show, Read, Eq, Ord, Generic)

startIndex :: Index
startIndex = Index 0

succIndex :: Index -> Index
succIndex (Index i) = Index (succ i)

data Config nt = Config
  { _nodeSet               :: Set nt
  , _nodeId                :: nt
  , _electionTimeoutRange  :: (Int,Int) -- in microseconds
  , _heartbeatTimeout      :: Int -- in microseconds
  }
  deriving (Show, Generic)
makeLenses ''Config

data AppendEntries nt et = AppendEntries
  { _aeTerm :: Term
  , _leaderId :: nt
  , _prevLogIndex :: Index
  , _prevLogTerm :: Term
  , _entries :: [et] -- TODO: maybe not a list
  , _leaderCommit :: Index
  }
  deriving (Show, Read, Generic)
makeLenses ''AppendEntries

data AppendEntriesResponse = AppendEntriesResponse
  { _aerTerm :: Term
  , _success :: Bool
  }
  deriving (Show, Read, Generic)
makeLenses ''AppendEntriesResponse

data RequestVote nt = RequestVote
  { _rvTerm :: Term
  , _candidateId :: nt
  , _lastLogIndex :: Index
  , _lastLogTerm :: Term
  }
  deriving (Show, Read, Generic)
makeLenses ''RequestVote

data RequestVoteResponse = RequestVoteResponse
  { _rvrTerm :: Term
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
                  | AER AppendEntriesResponse
                  | RV (RequestVote nt)
                  | RVR RequestVoteResponse
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
  , __writeLogEntry    :: Index -> et -> IO ()

    -- ^ Function to get the term number from persistent storage.
  , __readTermNumber   :: IO Term

    -- ^ Function to write the term number to persistent storage.
  , __writeTermNumber  :: Term -> IO ()

    -- ^ Function to read the node voted for from persistent storage.
  , __readVotedFor     :: IO (Maybe nt)

    -- ^ Function to write the node voted for to persistent storage.
  , __writeVotedFor    :: nt -> IO ()

    -- ^ Function to commit a log entry.
  , __commitLogEntry   :: et -> IO rt

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
  }

data CandidateState nt = CandidateState
  { _votes  :: Map nt ByteString
  }
  deriving (Show, Generic)
makeLenses ''CandidateState

data LeaderState nt = LeaderState
  { _nextIndex  :: Map nt Index
  , _matchIndex :: Map nt Index
  }
  deriving (Show, Generic)
makeLenses ''LeaderState

data Role nt = Follower
             | Candidate (CandidateState nt)
             | Leader    (LeaderState    nt)
  deriving (Show, Generic)

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
  , _writeLogEntry    :: MonadTrans t => Index -> et -> t IO ()

    -- ^ Function to get the term number from persistent storage.
  , _readTermNumber   :: MonadTrans t => t IO Term

    -- ^ Function to write the term number to persistent storage.
  , _writeTermNumber  :: MonadTrans t => Term -> t IO ()

    -- ^ Function to read the node voted for from persistent storage.
  , _readVotedFor     :: MonadTrans t => t IO (Maybe nt)

    -- ^ Function to write the node voted for to persistent storage.
  , _writeVotedFor    :: MonadTrans t => nt -> t IO ()

    -- ^ Function to commit a log entry.
  , _commitLogEntry   :: MonadTrans t => et -> t IO rt

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
    , _commitLogEntry  = lift . __commitLogEntry
    , _openConnection  = lift . __openConnection
    , _serializeRPC    = __serializeRPC
    , _deserializeRPC  = __deserializeRPC
    , _sendMessage     = \n m -> lift (__sendMessage n m)
    , _getMessage      = lift . __getMessage
    }

data RaftState nt = RaftState
  { _role        :: Role nt
  , _commitIndex :: Index
  , _lastApplied :: Index
  , _timerThread :: Maybe ThreadId
  }
  deriving (Show, Generic)
makeLenses ''RaftState

data RaftEnv nt et rt mt ht = RaftEnv
  { _cfg      :: Config nt
  , _conn     :: ht
  , _eventIn  :: InChan (Event mt)
  , _eventOut :: OutChan (Event mt)
  , _rs       :: LiftedRaftSpec nt et rt mt ht (RWST (RaftEnv nt et rt mt ht) () (RaftState nt))
  }
makeLenses ''RaftEnv

type Raft nt et rt mt ht a = RWST (RaftEnv nt et rt mt ht) () (RaftState nt) IO a

-- let all the RPC's have a single lens called term
class MessageTerm m where
  term :: Functor f => (Term -> f Term) -> m -> f m
instance MessageTerm (AppendEntries nt et) where
  term = aeTerm
instance MessageTerm AppendEntriesResponse where
  term = aerTerm
instance MessageTerm (RequestVote nt) where
  term = rvTerm
instance MessageTerm RequestVoteResponse where
  term = rvrTerm

instance Binary Term
instance Binary Index

instance (Binary nt, Binary et) => Binary (AppendEntries nt et)
instance Binary AppendEntriesResponse
instance Binary nt => Binary (RequestVote nt)
instance Binary RequestVoteResponse
instance Binary et => Binary (Command et)

instance (Binary nt, Binary et, Binary rt) => Binary (RPC nt et rt)
