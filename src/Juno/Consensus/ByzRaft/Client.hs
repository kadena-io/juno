{-# LANGUAGE RecordWildCards #-}

module Juno.Consensus.ByzRaft.Client
  ( runRaftClient
  ) where

import Control.Lens hiding (Index)
import Control.Monad.RWS

import Data.Foldable (traverse_)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Thyme.Clock
import Data.Thyme.Time.Core (unUTCTime, toMicroseconds)

import Juno.Runtime.Timer
import Juno.Runtime.Types
import Juno.Util.Util
import Juno.Runtime.Sender (sendSignedRPC)

import qualified Control.Concurrent.Lifted as CL

runRaftClient :: IO CommandEntry
              -> (CommandResult -> IO ())
              -> Config
              -> RaftSpec (Raft IO)
              -> IO ()
runRaftClient getEntry useResult rconf spec@RaftSpec{..} = do
  let qsize = getQuorumSize $ Set.size $ rconf ^. otherNodes
  UTCTime _ time <- liftIO $ unUTCTime <$> getCurrentTime
  runRWS_
    (raftClient (lift getEntry) (lift . useResult))
    (RaftEnv rconf qsize spec)
    initialRaftState {_currentRequestId = toRequestId $ toMicroseconds time }-- only use currentLeader and logEntries

-- THREAD: CLIENT MAIN
raftClient :: Raft IO CommandEntry -> (CommandResult -> Raft IO ()) -> Raft IO ()
raftClient getEntry useResult = do
  nodes <- view (cfg.otherNodes)
  when (Set.null nodes) $ error "The client has no nodes to send requests to."
  currentLeader .= (Just $ Set.findMin nodes)
  void $ CL.fork messageReceiver -- THREAD: CLIENT MESSAGE RECEIVER
  void $ CL.fork $ commandGetter getEntry -- THREAD: CLIENT COMMAND
  pendingRequests .= Map.empty
  clientHandleEvents useResult

-- get commands with getEntry and put them on the event queue to be sent
-- THREAD: CLIENT COMMAND
commandGetter :: Monad m => Raft m CommandEntry -> Raft m ()
commandGetter getEntry = do
  nid <- view (cfg.nodeId)
  forever $ do
    entry <- getEntry
    rid <- nextRequestId
    enqueueEvent $ ERPC $ CMD $ Command entry nid rid B.empty

-- THREAD: CLIENT COMMAND. updates state!
nextRequestId :: Monad m => Raft m RequestId
nextRequestId = do
  currentRequestId += 1
  use currentRequestId

-- THREAD: CLIENT MAIN. updates state
clientHandleEvents :: Monad m => (CommandResult -> Raft m ()) -> Raft m ()
clientHandleEvents useResult = forever $ do
  e <- dequeueEvent
  case e of
    ERPC (CMD cmd)     -> clientSendCommand cmd -- these are commands coming from the commandGetter thread
    ERPC (CMDR cmdr)   -> clientHandleCommandResponse useResult cmdr
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
      pendingRequests %= Map.insert _cmdRequestId cmd
    Nothing  -> do
      setLeaderToFirst
      clientSendCommand cmd

-- THREAD: CLIENT MAIN. updates state
clientHandleCommandResponse :: Monad m => (CommandResult -> Raft m ()) -> CommandResponse -> Raft m ()
clientHandleCommandResponse useResult cmdr@CommandResponse{..} = do
  prs <- use pendingRequests
  valid <- verifyRPCWithKey (CMDR cmdr)
  when (valid && Map.member _cmdrRequestId prs) $ do
    useResult _cmdrResult
    currentLeader .= Just _cmdrLeaderId
    pendingRequests %= Map.delete _cmdrRequestId
    numTimeouts .= 0
    prcount <- fmap Map.size (use pendingRequests)
    -- if we still have pending requests, reset the timer
    -- otherwise cancel it
    if (prcount > 0)
      then resetHeartbeatTimer
      else cancelTimer
