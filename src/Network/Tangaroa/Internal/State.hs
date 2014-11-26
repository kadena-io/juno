module Network.Tangaroa.Internal.State
  ( initialVolatileState
  , initialCandidateState
  , initialLeaderState
  ) where

import Data.Map as Map

import Network.Tangaroa.Types

initialVolatileState :: VolatileState nt
initialVolatileState = VolatileState Follower startIndex startIndex

initialCandidateState :: CandidateState nt
initialCandidateState = CandidateState Map.empty

initialLeaderState :: LeaderState nt
initialLeaderState = LeaderState Map.empty Map.empty

