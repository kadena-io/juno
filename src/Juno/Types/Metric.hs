
-----------------------------------------------------------------------------
-- |
-- Module      :  Juno.Runtime.Protocol.Types
--
-- Holds the core Juno/Raft Types used to implement BFT Raft
-- the types here are internal to the protocol nodes, but for now they share
-- some types Runtime/Types.hs with Api/Types.hs.

module Juno.Types.Metric
  ( Metric(..)
  ) where

import Data.ByteString (ByteString)

import Juno.Types.Base

data Metric
  -- Consensus metrics:
  = MetricTerm Term
  | MetricCommitIndex LogIndex
  | MetricCommitPeriod Double          -- For computing throughput
  | MetricCurrentLeader (Maybe NodeID)
  | MetricHash ByteString
  -- Node metrics:
  | MetricNodeId NodeID
  | MetricRole Role
  | MetricAppliedIndex LogIndex
  | MetricApplyLatency Double
  -- Cluster metrics:
  | MetricClusterSize Int
  | MetricQuorumSize Int
  | MetricAvailableSize Int
