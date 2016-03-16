{-# LANGUAGE RecordWildCards #-}

module Juno.Consensus.ByzRaft.Client
  ( runRaftClient
   ,CommandMVarMap
   ,setNextCmdRequestId
  ) where

import Control.Lens hiding (Index)
import Control.Monad.RWS

import Data.Foldable (traverse_)
import qualified Data.ByteString.Lazy as B
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Thyme.Clock
import Data.Thyme.Time.Core (unUTCTime, toMicroseconds)

import Juno.Runtime.Timer
import Juno.Runtime.Types
import Juno.Util.Util
import Juno.Runtime.Sender (sendSignedRPC)

import           Control.Concurrent (MVar, modifyMVar_, takeMVar, putMVar)
import qualified Control.Concurrent.Lifted as CL

-- shared holds the command result when the status is CmdApplied
type CommandMVarMap = MVar (Map RequestId CommandStatus)

-- move to utils, this is the only CommandStatus that should inc the requestId
setNextCmdRequestId :: CommandMVarMap -> IO (RequestId, CommandMVarMap)
setNextCmdRequestId cmdStatusMap = do
         m <- takeMVar cmdStatusMap
         let nextId = if (Map.size m == 0)
                      then 1
                      else (fst $ Map.findMax m) + 1 -- O(log n)
         putMVar cmdStatusMap (Map.insert nextId CmdSubmitted m)
         return (nextId, cmdStatusMap)

-- main entry point wired up by Simple.hs
-- getEntry (readChan) useResult (writeChan) replace by
-- CommandMVarMap (MVar shared with App client)
runRaftClient :: IO (RequestId, CommandEntry) -> CommandMVarMap -> Config -> RaftSpec (Raft IO) -> IO ()
runRaftClient getEntry cmdStatusMap rconf spec@RaftSpec{..} = do
  let qsize = getQuorumSize $ Set.size $ rconf ^. otherNodes
  UTCTime _ time <- liftIO $ unUTCTime <$> getCurrentTime
  runRWS_
    (raftClient (lift getEntry) cmdStatusMap)
    (RaftEnv rconf qsize spec)
    initialRaftState {_currentRequestId = toRequestId $ toMicroseconds time }-- only use currentLeader and logEntries

-- THREAD: CLIENT MAIN
raftClient :: Raft IO (RequestId, CommandEntry) -> CommandMVarMap -> Raft IO ()
raftClient getEntry cmdStatusMap = do
  nodes <- view (cfg.otherNodes)
  when (Set.null nodes) $ error "The client has no nodes to send requests to."
  currentLeader .= (Just $ Set.findMin nodes)
  void $ CL.fork messageReceiver -- THREAD: CLIENT MESSAGE RECEIVER
  void $ CL.fork $ commandGetter getEntry cmdStatusMap -- THREAD: CLIENT COMMAND REPL?
  pendingRequests .= Map.empty
  clientHandleEvents cmdStatusMap -- forever read chan loop

-- get commands with getEntry and put them on the event queue to be sent
-- THREAD: CLIENT COMMAND
commandGetter :: MonadIO m => Raft m (RequestId, CommandEntry) -> CommandMVarMap -> Raft m ()
commandGetter getEntry cmdStatusMap = do
  nid <- view (cfg.nodeId)
  forever $ do
    (rid, entry) <- getEntry
    rid' <- setNextRequestId rid -- set current requestId to the value associated with this request.
    liftIO (modifyMVar_ cmdStatusMap (\m -> return (Map.insert rid CmdAccepted m)))
    enqueueEvent $ ERPC $ CMD $ Command entry nid rid' B.empty

-- THREAD: CLIENT COMMAND. updates state!
-- TODO: used in revolution, should take this from MVar map as well?
nextRequestId :: Monad m => Raft m RequestId
nextRequestId = do
  currentRequestId += 1
  use currentRequestId

setNextRequestId :: Monad m => RequestId -> Raft m RequestId
setNextRequestId rid = do
  currentRequestId .= rid
  use currentRequestId

-- THREAD: CLIENT MAIN. updates state
clientHandleEvents :: MonadIO m => CommandMVarMap -> Raft m ()
clientHandleEvents cmdStatusMap = forever $ do
  e <- dequeueEvent -- blocking queue
  case e of
    ERPC (CMD cmd)     -> clientSendCommand cmd -- RPC + heartbeat + electionTimeout these are commands coming from the commandGetter thread
    ERPC (CMDR cmdr)   -> clientHandleCommandResponse cmdStatusMap cmdr
    HeartbeatTimeout _ -> do
      timeouts <- use numTimeouts
      limit <- view (cfg.clientTimeoutLimit)
      if timeouts < limit
        then do
          debug "choosing a new leader and resending commands"
          setLeaderToNext
          reqs <- use pendingRequests
          pendingRequests .= Map.empty -- this will reset the timer on resend
          traverse_ clientSendCommand reqs
          numTimeouts += 1
        else do
          debug "starting a revolution"
          nid <- view (cfg.nodeId)
          mlid <- use currentLeader
          case mlid of
            Just lid -> do
              rid <- nextRequestId
              view (cfg.otherNodes) >>=
                traverse_ (\n -> sendSignedRPC n (REVOLUTION (Revolution nid lid rid B.empty)))
              numTimeouts .= 0
              resetHeartbeatTimer
            _ -> do
              setLeaderToFirst
              resetHeartbeatTimer
    _                  -> return ()

-- THREAD: CLIENT MAIN. updates state
-- If the client doesn't know the leader? Then set leader to first node, the client will be updated with the real leaderId when it receives a command response.
setLeaderToFirst :: Monad m => Raft m ()
setLeaderToFirst = do
  nodes <- view (cfg.otherNodes)
  when (Set.null nodes) $ error "the client has no nodes to send requests to"
  currentLeader .= (Just $ Set.findMin nodes)

-- THREAD: CLIENT MAIN. updates state.
setLeaderToNext :: Monad m => Raft m ()
setLeaderToNext = do
  mlid <- use currentLeader
  nodes <- view (cfg.otherNodes)
  case mlid of
    Just lid -> case Set.lookupGT lid nodes of
      Just nlid -> currentLeader .= Just nlid
      Nothing   -> setLeaderToFirst
    Nothing -> setLeaderToFirst

-- THREAD: CLIENT MAIN. updates state
clientSendCommand :: Monad m => Command -> Raft m ()
clientSendCommand cmd@Command{..} = do
  mlid <- use currentLeader
  case mlid of
    Just lid -> do
      sendSignedRPC lid $ CMD cmd
      prcount <- fmap Map.size (use pendingRequests)
      -- if this will be our only pending request, start the timer
      -- otherwise, it should already be running
      when (prcount == 0) resetHeartbeatTimer
      pendingRequests %= Map.insert _cmdRequestId cmd -- TODO should we update CommandMap here?
    Nothing  -> do
      setLeaderToFirst
      clientSendCommand cmd

-- THREAD: CLIENT MAIN. updates state
-- Command returned has been applied
clientHandleCommandResponse :: MonadIO m => CommandMVarMap -> CommandResponse -> Raft m ()
clientHandleCommandResponse cmdStatusMap cmdr@CommandResponse{..} = do
  prs <- use pendingRequests
  valid <- verifyRPCWithKey (CMDR cmdr)
  when (valid && Map.member _cmdrRequestId prs) $ do
    currentLeader .= Just _cmdrLeaderId
    pendingRequests %= Map.delete _cmdrRequestId
    -- cmdStatusMap shared with the client, client can poll this map to await applied result
    liftIO (modifyMVar_ cmdStatusMap (\m -> return (Map.insert _cmdrRequestId (CmdApplied _cmdrResult ) m)))
    numTimeouts .= 0
    prcount <- fmap Map.size (use pendingRequests)
    -- if we still have pending requests, reset the timer
    -- otherwise cancel it
    if (prcount > 0)
      then resetHeartbeatTimer
      else cancelTimer
