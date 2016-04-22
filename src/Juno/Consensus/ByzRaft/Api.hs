{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Juno.Consensus.ByzRaft.Api
  ( apiReceiver
  ) where

import           Control.Lens
import qualified Data.Set as Set
import           Control.Monad
import           Juno.Runtime.Types
import           Juno.Runtime.Protocol.Types
import           Juno.Util.Util
import           Juno.Runtime.Timer

import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as SB8
import           Control.Monad.RWS
import           Text.Read (readMaybe)
import           Control.Concurrent (takeMVar, putMVar, modifyMVar_)
import           Juno.Runtime.Sender (sendRPC)

-- TODO do we need all this? can we just enqueueEvent directly?
-- get commands with getEntry and put them on the event queue to be sent
-- THREAD: CLIENT COMMAND
apiReceiver :: MonadIO m => Raft m ()
apiReceiver = do
  nid <- view (cfg.nodeId)
  forever $ do
    cmdMap <- view (rs.cmdStatusMap)
    (rid@(RequestId _), cmdEntries) <- dequeueCommand
    -- support for special REPL command "> batch test:5000", runs hardcoded batch job
    cmds' <- case cmdEntries of
               (CommandEntry cmd):[] | SB8.take 11 cmd == "batch test:" -> do
                                          let missiles = take (batchSize cmd) $ repeat $ hardcodedTransfers nid cmdMap
                                          liftIO $ sequence $ missiles
               _ -> liftIO $ sequence $ fmap ((nextRid nid) cmdMap) cmdEntries
    -- set current requestId in Raft to the value associated with this request.
    rid' <- setNextRequestId' rid
    liftIO (modifyMVar_ cmdMap (\(CommandMap n m) -> return $ CommandMap n (Map.insert rid CmdAccepted m)))
    -- hack set the head to the org rid
    let cmds'' = case cmds' of
                   ((Command entry nid' _ NewMsg):rest) -> (Command entry nid' rid' NewMsg):rest
                   _ -> []
    -- TODO: have the client really sign this and map the client digest to this.
    --       for now, node 1003 has keys registered as client and protocol node.
    clientSendCommandBatch' $ CommandBatch cmds'' NewMsg
  where
    batchSize :: (Num c, Read c) => SB8.ByteString -> c
    batchSize cmd = maybe 500 id . readMaybe $ drop 11 $ SB8.unpack cmd

    nextRid :: NodeID -> CommandMVarMap -> CommandEntry -> IO Command
    nextRid nid cmdMap entry = do
      rid <- (setNextCmdRequestId' cmdMap)
      return (Command entry nid rid NewMsg)

    hardcodedTransfers :: NodeID -> CommandMVarMap-> IO Command
    hardcodedTransfers nid cmdMap = nextRid nid cmdMap transferCmdEntry

    transferCmdEntry :: CommandEntry
    transferCmdEntry = (CommandEntry "transfer(Acct1->Acct2, 1 % 1)")

-- move to utils, this is the only CommandStatus that should inc the requestId
-- NB: this only works when we have a single client, but punting on solving this for now is a good idea.
-- TODO add Mac or node ID to the requestID for now.
setNextCmdRequestId' :: CommandMVarMap -> IO RequestId
setNextCmdRequestId' cmdMapMvar = do
  (CommandMap nextId m) <- takeMVar cmdMapMvar
  putMVar cmdMapMvar $ CommandMap (nextId + 1) (Map.insert nextId CmdSubmitted m)
  return nextId

-- This should be broken now? This node might not be the leader.
setNextRequestId' :: Monad m => RequestId -> Raft m RequestId
setNextRequestId' rid = do
  currentRequestId .= rid
  use currentRequestId

-- | Always send CommandBatches, a single Command is a batch of size 1.
-- Sends to the leader, knows the leader because running in Raft
-- THREAD: CLIENT MAIN. updates state
clientSendCommandBatch' :: Monad m => CommandBatch -> Raft m ()
clientSendCommandBatch' cmdb@CommandBatch{..} = do
  mlid <- use currentLeader
  case mlid of
    Just lid -> do
      sendRPC lid $ CMDB' cmdb
      prcount <- fmap Map.size (use pendingRequests)
      -- if this will be our only pending request, start the timer
      -- otherwise, it should already be running
      let lastCmd = last _cmdbBatch
      when (prcount == 0) resetHeartbeatTimer
      pendingRequests %= Map.insert (_cmdRequestId lastCmd) lastCmd -- TODO should we update CommandMap here?
    Nothing  -> do
      setLeaderToFirst' -- TODO: do we need this anymore? The raft protocol should be taking care of this as well.
      clientSendCommandBatch' cmdb

-- THREAD: CLIENT MAIN. updates state
-- If the client doesn't know the leader? Then set leader to first node, the client will be updated with the real leaderId when it receives a command response.
setLeaderToFirst' :: Monad m => Raft m ()
setLeaderToFirst' = do
  nodes <- view (cfg.otherNodes)
  when (Set.null nodes) $ error "the client has no nodes to send requests to"
  setCurrentLeader $ Just $ Set.findMin nodes

-- TODO: remove these (still in Client.hs)
--       this only need to be command related, there should not longer be code
--       for handling other events, this is done by the protocol.
-- THREAD: CLIENT MAIN. updates state.
setLeaderToNext' :: Monad m => Raft m ()
setLeaderToNext' = do
  mlid <- use currentLeader
  nodes <- view (cfg.otherNodes)
  case mlid of
    Just lid -> case Set.lookupGT lid nodes of
      Just nlid -> setCurrentLeader $ Just nlid
      Nothing   -> setLeaderToFirst'
    Nothing -> setLeaderToFirst'
