{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Juno.Monitoring.Server
  ( startMonitoring
  ) where

import Juno.Runtime.Types (Config, Metric(..), LogIndex(..), Term(..),
                           NodeID(..), nodeId, _port)

import System.Remote.Monitoring (Server, forkServer, getLabel, getGauge)
import Control.Lens ((^.), to)

import qualified Data.Text as T
import qualified System.Metrics.Label as Label
import qualified System.Metrics.Gauge as Gauge

-- TODO: probably switch to 'newStore' API. this allows us to use groups.

startApi :: Config -> IO Server
startApi config = forkServer "localhost" port
  where
    -- TODO: change this port / load it from config
    port = 80 + fromIntegral (config ^. nodeId . to _port)

startMonitoring :: Config -> IO (Metric -> IO ())
startMonitoring config = do
  ekg <- startApi config

  -- Consensus
  termGauge <- getGauge "juno.consensus.term" ekg
  logIndexGauge <- getGauge "juno.consensus.log_index" ekg
  commitIndexGauge <- getGauge "juno.consensus.commit_index" ekg
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
    MetricLogIndex (LogIndex idx) ->
      Gauge.set logIndexGauge $ fromIntegral idx
    MetricCommitIndex (LogIndex idx) ->
      Gauge.set commitIndexGauge $ fromIntegral idx
    -- Node
    MetricNodeId (NodeID host port) -> do
      Label.set nodeIdLabel $ T.pack $ host ++ ":" ++ show port
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
