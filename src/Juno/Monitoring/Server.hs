{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Juno.Monitoring.Server
  ( startMonitoring
  ) where

import Juno.Runtime.Types (Config, Metric(..), LogIndex(..), Term(..),
                           NodeID(..), nodeId, _port)

import System.Remote.Monitoring (Server, forkServer, getLabel, getGauge,
                                 getDistribution)
import Control.Lens ((^.), to)

import qualified Data.Text as T
import qualified System.Metrics.Label as Label
import qualified System.Metrics.Gauge as Gauge
import qualified System.Metrics.Distribution as Distribution

-- TODO: possibly switch to 'newStore' API. this allows us to use groups.

startApi :: Config -> IO Server
startApi config = forkServer "localhost" port
  where
    port = 80 + fromIntegral (config ^. nodeId . to _port)

startMonitoring :: Config -> IO (Metric -> IO ())
startMonitoring config = do
  ekg <- startApi config

  -- Consensus
  termGauge <- getGauge "juno.consensus.term" ekg
  commitIndexGauge <- getGauge "juno.consensus.commit_index" ekg
  commitPeriodDist <- getDistribution "juno.consensus.commit_period" ekg
  currentLeaderLabel <- getLabel "juno.consensus.current_leader" ekg
  -- Node
  nodeIdLabel <- getLabel "juno.node.id" ekg
  hostLabel <- getLabel "juno.node.host" ekg
  portGauge <- getGauge "juno.node.port" ekg
  roleLabel <- getLabel "juno.node.role" ekg
  appliedIndexGauge <- getGauge "juno.node.applied_index" ekg
  -- Cluster
  clusterSizeGauge <- getGauge "juno.cluster.size" ekg
  quorumSizeGauge <- getGauge "juno.cluster.quorum_size" ekg

  return $ \case
    -- Consensus
    MetricTerm (Term t) ->
      Gauge.set termGauge $ fromIntegral t
    MetricCommitIndex (LogIndex idx) ->
      Gauge.set commitIndexGauge $ fromIntegral idx
    MetricCommitPeriod p ->
      Distribution.add commitPeriodDist p
    MetricCurrentLeader mNode ->
      case mNode of
        Just node -> Label.set currentLeaderLabel $ nodeDescription node
        Nothing -> Label.set currentLeaderLabel ""
    -- Node
    MetricNodeId node@(NodeID host port) -> do
      Label.set nodeIdLabel $ nodeDescription node
      Label.set hostLabel $ T.pack host
      Gauge.set portGauge $ fromIntegral port
    MetricRole role ->
      Label.set roleLabel $ T.pack $ show role
    MetricAppliedIndex (LogIndex idx) ->
      Gauge.set appliedIndexGauge $ fromIntegral idx
    -- Cluster
    MetricClusterSize size ->
      Gauge.set clusterSizeGauge $ fromIntegral size
    MetricQuorumSize size ->
      Gauge.set quorumSizeGauge $ fromIntegral size

  where
    nodeDescription :: NodeID -> T.Text
    nodeDescription (NodeID host port) = T.pack $ host ++ ":" ++ show port
