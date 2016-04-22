
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
