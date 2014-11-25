{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Tangaroa.Types
  ( Term, startTerm, nextTerm
  , Index, startIndex, nextIndex
  , Config(..), cfgNodeSet, cfgNodeId, cfgElectionTimeout, cfgHeartbeatTimeout
  , FollowerState(..), fLeader
  , CandidateState(..), cVotes
  , LeaderState(..), nextIndexMap, matchIndexMap
  , Role(..)
  , PersistentState(..), currentTerm, votedFor, logEntries
  , VolatileState(..), role, commitIndex, lastApplied
  , AppendEntries(..), aeTerm, leaderId, prevLogIndex, entries, leaderCommit
  , AppendEntriesResponse(..), aerTerm, success
  , RequestVote(..), rvTerm, candidateId, lastLogIndex, lastLogTerm
  , RequestVoteResponse(..), rvrTerm, voteGranted
  , RPC(..)
  , term
  ) where

import Control.Lens hiding (Index)

import Data.Word
import Data.Set (Set)
import Data.Map (Map)
import Data.ByteString (ByteString)

import Data.Binary

import GHC.Generics

newtype Term = Term Word64
  deriving (Show, Read, Eq, Ord, Generic)

startTerm :: Term
startTerm = Term 0

nextTerm :: Term -> Term
nextTerm (Term t) = Term (succ t)

newtype Index = Index Word64
  deriving (Show, Read, Eq, Ord, Generic)

startIndex :: Index
startIndex = Index 0

nextIndex :: Index -> Index
nextIndex (Index i) = Index (succ i)

data Config nt et = Config
  { _cfgNodeSet          :: Set nt
  , _cfgNodeId           :: nt
  , _cfgElectionTimeout  :: Int -- in microseconds
  , _cfgHeartbeatTimeout :: Int -- in microseconds
  }
  deriving (Show, Generic)
makeLenses ''Config

data FollowerState nt et = FollowerState
  { _fLeader :: nt
  }
  deriving (Show, Generic)
makeLenses ''FollowerState

data CandidateState nt et = CandidateState
  { _cVotes  :: Map nt ByteString
  }
  deriving (Show, Generic)
makeLenses ''CandidateState

data LeaderState nt et = LeaderState
  { _nextIndexMap  :: Map nt Index
  , _matchIndexMap :: Map nt Index
  }
  deriving (Show, Generic)
makeLenses ''LeaderState

data Role nt et = Follower  (FollowerState  nt et)
                | Candidate (CandidateState nt et)
                | Leader    (LeaderState    nt et)
  deriving (Show, Generic)

data PersistentState nt et = PersistentState
  { _currentTerm :: Term
  , _votedFor    :: Maybe nt
  , _logEntries  :: [et] -- TODO: maybe not a list?
  }
  deriving (Show, Generic)
makeLenses ''PersistentState

data VolatileState nt et = VolatileState
  { _role        :: Role nt et
  , _commitIndex :: Index
  , _lastApplied :: Index
  }
  deriving (Show, Generic)
makeLenses ''VolatileState

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

data RPC nt et = AE (AppendEntries nt et)
               | AER AppendEntriesResponse
               | RV (RequestVote nt)
               | RVR RequestVoteResponse
  deriving (Show, Read, Generic)

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
instance (Binary nt, Binary et) => Binary (PersistentState nt et)

instance (Binary nt, Binary et) => Binary (AppendEntries nt et)
instance Binary AppendEntriesResponse
instance Binary nt => Binary (RequestVote nt)
instance Binary RequestVoteResponse

instance (Binary nt, Binary et) => Binary (RPC nt et)
