{-# LANGUAGE OverloadedStrings #-}

module Juno.Runtime.Api.ApiServer (runApiServer) where

-- | Api server is the interface between outside clients
--   and the internal Juno/Raft protocol.
--   Responible for:
--   * listening for incoming Commands
--   * managing RequestId
--   * check status of command, i.e. communication between API server and Raft (MVar for now, but this could be redis or another key value store running on every node
--   * Parse Commands, and change CommandEntry -> [Command (with nodeId)] .. should this happen here? Or should that be in the protocol?

import           Juno.Runtime.Types hiding (Config)

import           Apps.Juno.ApiHandlers
import           Control.Applicative
import           Control.Concurrent.Chan.Unagi
import           Control.Monad.Reader

import           Snap.Http.Server
import           Snap.Core
import           Snap.CORS

-- |
-- Starts the API server which will listen on a port for incoming client
-- commands (raw commands are written to the `toCommands` channel) in the form:
-- `"AdjustAccount " ++ T.unpack acct ++ " " ++ show (toRational amt)`.
-- The Juno / raft protocol will be initialized with out/read side of the
-- this channel, and is responisble for putting the data in the correct
-- format for the protocol. For now when querying the only shared item
-- sharedCmdStatusMap
runApiServer :: InChan (RequestId, [CommandEntry]) -> CommandMVarMap -> Int -> IO ()
runApiServer toCommands sharedCmdStatusMap port = do
  putStrLn "Starting up server runApiServer"
  snapApiServer toCommands sharedCmdStatusMap port
  putStrLn "Server Started"

-- TODO removed from App/client
snapApiServer :: InChan (RequestId, [CommandEntry]) -> CommandMVarMap -> Int -> IO ()
snapApiServer toCommands cmdStatusMap port = httpServe (serverConf port) $
    applyCORS (defaultOptions) $ methods [GET, POST]
    (ifTop (writeBS "use /hopper for commands") <|>
     route [ ("/api/juno/v1", runReaderT apiRoutes (ApiEnv toCommands cmdStatusMap))]
    )

serverConf :: MonadSnap m => Int -> Config m a
serverConf port = setErrorLog (ConfigFileLog "log/error.log") $ setAccessLog (ConfigFileLog "log/access.log") $ setPort port $ defaultConfig
