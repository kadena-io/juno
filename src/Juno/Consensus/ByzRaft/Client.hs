{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Juno.Consensus.ByzRaft.Client
  ( runRaftClient
  ) where

import Control.Lens hiding (Index)
import Control.Monad.RWS
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Read (readMaybe)
import qualified Data.ByteString.Char8 as SB8

import Juno.Runtime.Timer
import Juno.Types
import Juno.Util.Util
import Juno.Runtime.Sender (sendRPC)
import Juno.Runtime.MessageReceiver

import           Control.Concurrent (modifyMVar_, readMVar)
import qualified Control.Concurrent.Lifted as CL

-- main entry point wired up by Simple.hs
-- getEntry (readChan) useResult (writeChan) replace by
-- CommandMVarMap (MVar shared with App client)
runRaftClient :: IO (RequestId, [CommandEntry])
              -> CommandMVarMap
              -> Config
              -> RaftSpec (Raft IO)
              -> IO ()
runRaftClient getEntries cmdStatusMap' rconf spec@RaftSpec{..} = do
  let csize = Set.size $ rconf ^. otherNodes
      qsize = getQuorumSize csize
  -- TODO: do we really need currentRequestId in state any longer, doing this to keep them in sync
  (CommandMap rid _) <- readMVar cmdStatusMap'
  runRWS_
    (raftClient (lift getEntries) cmdStatusMap')
    (RaftEnv rconf csize qsize spec)
    -- TODO: because UTC can flow backwards, this request ID is problematic:
    initialRaftState {_currentRequestId = rid}-- only use currentLeader and logEntries

-- TODO: don't run in raft, own monad stack
-- StateT ClientState (ReaderT ClientEnv IO)
-- THREAD: CLIENT MAIN
raftClient :: Raft IO (RequestId, [CommandEntry]) -> CommandMVarMap -> Raft IO ()
raftClient getEntries cmdStatusMap' = do
  nodes <- view (cfg.otherNodes)
  when (Set.null nodes) $ error "The client has no nodes to send requests to."
  setCurrentLeader $ Just $ Set.findMin nodes
  void $ CL.fork messageReceiver -- THREAD: CLIENT MESSAGE RECEIVER
  void $ CL.fork $ commandGetter getEntries cmdStatusMap' -- THREAD: CLIENT COMMAND REPL?
  pendingRequests .= Map.empty
  clientHandleEvents cmdStatusMap' -- forever read chan loop

-- get commands with getEntry and put them on the event queue to be sent
-- THREAD: CLIENT COMMAND
commandGetter :: MonadIO m => Raft m (RequestId, [CommandEntry]) -> CommandMVarMap -> Raft m ()
commandGetter getEntries cmdStatusMap' = do
  nid <- view (cfg.nodeId)
  forever $ do
    (rid@(RequestId _), cmdEntries) <- getEntries
    -- support for special REPL command "> batch test:5000", runs hardcoded batch job
    cmds' <- case cmdEntries of
               (CommandEntry cmd):[] | SB8.take 11 cmd == "batch test:" -> do
                                          let missiles = take (batchSize cmd) $ repeat $ hardcodedTransfers nid
                                          liftIO $ sequence $ missiles
               _ -> liftIO $ sequence $ fmap (nextRid nid) cmdEntries
    -- set current requestId in Raft to the value associated with this request.
    rid' <- setNextRequestId rid
    liftIO (modifyMVar_ cmdStatusMap' (\(CommandMap n m) -> return $ CommandMap n (Map.insert rid CmdAccepted m)))
    -- hack set the head to the org rid
    let cmds'' = case cmds' of
                   ((Command entry nid' _ NewMsg):rest) -> (Command entry nid' rid' NewMsg):rest
                   _ -> [] -- TODO: fix this
    enqueueEvent $ ERPC $ CMDB' $ CommandBatch cmds'' NewMsg
  where
    batchSize :: (Num c, Read c) => SB8.ByteString -> c
    batchSize cmd = maybe 500 id . readMaybe $ drop 11 $ SB8.unpack cmd

    nextRid :: NodeID -> CommandEntry -> IO Command
    nextRid nid entry = do
      rid <- (setNextCmdRequestId cmdStatusMap')
      return (Command entry nid rid NewMsg)

    hardcodedTransfers :: NodeID -> IO Command
    hardcodedTransfers nid = nextRid nid transferCmdEntry

    transferCmdEntry :: CommandEntry
    transferCmdEntry = (CommandEntry "transfer(Acct1->Acct2, 1 % 1)")

setNextRequestId :: Monad m => RequestId -> Raft m RequestId
setNextRequestId rid = do
  currentRequestId .= rid
  use currentRequestId

-- THREAD: CLIENT MAIN. updates state
clientHandleEvents :: MonadIO m => CommandMVarMap -> Raft m ()
clientHandleEvents cmdStatusMap' = forever $ do
  e <- dequeueEvent -- blocking queue
  case e of
    ERPC (CMDB' cmdb)   -> clientSendCommandBatch cmdb
    ERPC (CMD' cmd)     -> clientSendCommand cmd -- these are commands coming from the commandGetter thread
    ERPC (CMDR' cmdr)   -> clientHandleCommandResponse cmdStatusMap' cmdr
    HeartbeatTimeout _ -> do
      debug "caught a heartbeat"
      timeouts <- use numTimeouts
      if timeouts > 3
      then do
        debug "choosing a new leader and resending commands"
        setLeaderToNext
        reqs <- use pendingRequests
        pendingRequests .= Map.empty -- this will reset the timer on resend
        traverse_ clientSendCommand reqs
        numTimeouts += 1
      else resetHeartbeatTimer >> numTimeouts += 1
    _ -> return ()

-- THREAD: CLIENT MAIN. updates state
-- If the client doesn't know the leader? Then set leader to first node, the client will be updated with the real leaderId when it receives a command response.
setLeaderToFirst :: Monad m => Raft m ()
setLeaderToFirst = do
  nodes <- view (cfg.otherNodes)
  when (Set.null nodes) $ error "the client has no nodes to send requests to"
  setCurrentLeader $ Just $ Set.findMin nodes

-- THREAD: CLIENT MAIN. updates state.
setLeaderToNext :: Monad m => Raft m ()
setLeaderToNext = do
  mlid <- use currentLeader
  nodes <- view (cfg.otherNodes)
  case mlid of
    Just lid -> case Set.lookupGT lid nodes of
      Just nlid -> setCurrentLeader $ Just nlid
      Nothing   -> setLeaderToFirst
    Nothing -> setLeaderToFirst

-- THREAD: CLIENT MAIN. updates state
clientSendCommand :: Monad m => Command -> Raft m ()
clientSendCommand cmd@Command{..} = do
  mlid <- use currentLeader
  case mlid of
    Just lid -> do
      sendRPC lid $ CMD' cmd
      prcount <- fmap Map.size (use pendingRequests)
      -- if this will be our only pending request, start the timer
      -- otherwise, it should already be running
      when (prcount == 0) resetHeartbeatTimer
      pendingRequests %= Map.insert _cmdRequestId cmd -- TODO should we update CommandMap here?
    Nothing  -> do
      setLeaderToFirst
      clientSendCommand cmd

-- THREAD: CLIENT MAIN. updates state
clientSendCommandBatch :: Monad m => CommandBatch -> Raft m ()
clientSendCommandBatch cmdb@CommandBatch{..} = do
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
      setLeaderToFirst
      clientSendCommandBatch cmdb

-- THREAD: CLIENT MAIN. updates state
-- Command has been applied
clientHandleCommandResponse :: MonadIO m => CommandMVarMap -> CommandResponse -> Raft m ()
clientHandleCommandResponse cmdStatusMap' CommandResponse{..} = do
  prs <- use pendingRequests
  when (Map.member _cmdrRequestId prs) $ do
    setCurrentLeader $ Just _cmdrLeaderId
    pendingRequests %= Map.delete _cmdrRequestId
    -- cmdStatusMap shared with the client, client can poll this map to await applied result
    liftIO (modifyMVar_ cmdStatusMap' (\(CommandMap rid m) -> return $ CommandMap rid (Map.insert _cmdrRequestId (CmdApplied _cmdrResult) m)))
    numTimeouts .= 0
    prcount <- fmap Map.size (use pendingRequests)
    -- if we still have pending requests, reset the timer
    -- otherwise cancel it
    if prcount > 0
      then resetHeartbeatTimer
      else cancelTimer
