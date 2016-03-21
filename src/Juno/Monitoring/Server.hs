{-# LANGUAGE OverloadedStrings #-}

module Juno.Monitoring.Server
  ( runMonitoringApi
  ) where

import Juno.Runtime.Types (Config, nodeId, _port)

import System.Remote.Monitoring (forkServer)
import Control.Monad (void)
import Control.Lens ((^.), to)

runMonitoringApi :: Config -> IO ()
runMonitoringApi config = void $ forkServer "localhost" port
  where
    -- TODO: change this port / load it from config
    port = 80 + fromIntegral (config ^. nodeId . to _port)
