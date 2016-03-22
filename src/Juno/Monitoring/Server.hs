{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Juno.Monitoring.Server
  ( startMonitoring
  ) where

import Juno.Runtime.Types (Config, Metric(..), LogIndex(..), nodeId, _port)

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

  roleLabel <- getLabel "juno.node.role" ekg
  logIndexGauge <- getGauge "juno.consensus.log_index" ekg

  return $ \case
    MetricRole role ->
      Label.set roleLabel $ T.pack $ show role
    MetricLogIndex (LogIndex idx) ->
      Gauge.set logIndexGauge $ fromIntegral idx
