{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Tangaroa.Types
  ( RaftSpec(..), readCfg, readLogEntry, writeLogEntry, readTermNumber, writeTermNumber
  , readVotedFor, writeVotedFor, commit, openConnection, serializeRPC
  , deserializeRPC , sendMessage , getMessage
  , Term, startTerm, succTerm
  , Index, startIndex, succIndex
  , Config(..), nodeSet, nodeId, electionTimeoutRange, heartbeatTimeout
  , CandidateState(..), votes
  , LeaderState(..), nextIndex, matchIndex
  , Role(..)
  , RaftEnv(..), cfg, conn, eventIn, eventOut, rs
  , RaftState(..), role, commitIndex, lastApplied, timerThread
  , AppendEntries(..), aeTerm, leaderId, prevLogIndex, entries, leaderCommit
  , AppendEntriesResponse(..), aerTerm, success
  , RequestVote(..), rvTerm, candidateId, lastLogIndex, lastLogTerm
  , RequestVoteResponse(..), rvrTerm, voteGranted
  , Command(..), entry
  , RPC(..)
  , term
  , Event(..)
  ) where

import Control.Lens hiding (Index)

import Data.Word
import Data.Set (Set)
import Data.Map (Map)
import Data.ByteString (ByteString)

import Data.Binary

import GHC.Generics

import Control.Concurrent (ThreadId)
import Control.Concurrent.Chan.Unagi

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
    _readCfg          :: IO (Config nt)

    -- ^ Function to get a log entry from persistent storage.
  , _readLogEntry     :: Index -> IO et

    -- ^ Function to write a log entry to persistent storage.
  , _writeLogEntry    :: Index -> et -> IO ()

    -- ^ Function to get the term number from persistent storage.
  , _readTermNumber   :: IO Term

    -- ^ Function to write the term number to persistent storage.
  , _writeTermNumber   :: Term -> IO ()

    -- ^ Function to read the node voted for from persistent storage.
  , _readVotedFor      :: IO (Maybe nt)

    -- ^ Function to write the node voted for to persistent storage.
  , _writeVotedFor     :: nt -> IO ()

    -- ^ Function to commit a log entry.
  , _commit           :: et -> IO rt

    -- ^ Function to open a connection handle.
  , _openConnection   :: nt -> IO ht

    -- ^ Function to serialize an RPC.
  , _serializeRPC     :: RPC nt et rt -> mt

    -- ^ Function to deserialize an RPC.
  , _deserializeRPC   :: mt -> Maybe (RPC nt et rt)

    -- ^ Function to send a message to a node.
  , _sendMessage      :: nt -> mt -> IO ()

    -- ^ Function to get the next message.
  , _getMessage       :: ht -> IO mt
  }
makeLenses ''RaftSpec

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

data RaftEnv nt et rt mt ht = RaftEnv
  { _cfg      :: Config nt
  , _conn     :: ht
  , _eventIn  :: InChan (Event mt)
  , _eventOut :: OutChan (Event mt)
  , _rs       :: RaftSpec nt et rt mt ht
  }
makeLenses ''RaftEnv

data RaftState nt = RaftState
  { _role        :: Role nt
  , _commitIndex :: Index
  , _lastApplied :: Index
  , _timerThread :: Maybe ThreadId
  }
  deriving (Show, Generic)
makeLenses ''RaftState

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
