module Network.Tangaroa.Internal.State
  ( initialRaftState
  , initialCandidateState
  , initialLeaderState
  ) where

import Data.Map as Map

import Network.Tangaroa.Types

initialRaftState :: RaftState nt
initialRaftState = RaftState Follower startIndex startIndex Nothing

initialCandidateState :: CandidateState nt
initialCandidateState = CandidateState Map.empty

initialLeaderState :: LeaderState nt
initialLeaderState = LeaderState Map.empty Map.empty
