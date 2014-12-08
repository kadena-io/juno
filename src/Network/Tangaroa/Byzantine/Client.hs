{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Tangaroa.Byzantine.Client
  ( runRaftClient
  ) where

import Network.Tangaroa.Byzantine.Timer
import Network.Tangaroa.Byzantine.Types
import Network.Tangaroa.Byzantine.Util
import Network.Tangaroa.Byzantine.Sender (sendRPC)

import Control.Concurrent.Chan.Unagi
import Control.Lens hiding (Index)
import Control.Monad.RWS
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Foldable (traverse_)

runRaftClient :: Ord nt => IO et -> (rt -> IO ()) -> Config nt -> RaftSpec nt et rt mt -> IO ()
runRaftClient getEntry useResult rconf spec@RaftSpec{..} = do
  let qsize = getQuorumSize $ Set.size $ rconf ^. otherNodes
  (ein, eout) <- newChan
  runRWS_
    (raftClient (lift getEntry) (lift . useResult))
    (RaftEnv rconf qsize ein eout (liftRaftSpec spec))
    initialRaftState -- only use currentLeader and logEntries

raftClient :: Ord nt => Raft nt et rt mt et -> (rt -> Raft nt et rt mt ()) -> Raft nt et rt mt ()
raftClient getEntry useResult = do
  nodes <- view (cfg.otherNodes)
  when (Set.null nodes) $ error "The client has no nodes to send requests to."
  currentLeader .= (Just $ Set.findMin nodes)
  fork_ messageReceiver
  fork_ $ commandGetter getEntry
  resetHeartbeatTimer -- use heartbeat events to trigger retransmissions
  pendingRequests .= Map.empty
  clientHandleEvents useResult

-- get commands with getEntry and put them on the event queue to be sent
commandGetter :: Raft nt et rt mt et -> Raft nt et rt mt ()
commandGetter getEntry = do
  nid <- view (cfg.nodeId)
  forever $ do
    entry <- getEntry
    rid <- use nextRequestId
    nextRequestId += 1
    enqueueEvent $ ERPC $ CMD $ Command entry nid rid

clientHandleEvents :: Ord nt => (rt -> Raft nt et rt mt ()) -> Raft nt et rt mt ()
clientHandleEvents useResult = forever $ do
  e <- dequeueEvent
  case e of
    ERPC (CMD cmd)     -> clientSendCommand cmd -- these are commands coming from the commandGetter thread
    ERPC (CMDR cmdr)   -> clientHandleCommandResponse useResult cmdr
    HeartbeatTimeout _ -> do
      debug "choosing a new leader and resending commands"
      setLeaderToNext
      traverse_ clientSendCommand =<< use pendingRequests
    _                  -> return ()

setLeaderToFirst :: Raft nt et rt mt ()
setLeaderToFirst = do
  nodes <- view (cfg.otherNodes)
  when (Set.null nodes) $ error "the client has no nodes to send requests to"
  currentLeader .= (Just $ Set.findMin nodes)

setLeaderToNext :: Ord nt => Raft nt et rt mt ()
setLeaderToNext = do
  mlid <- use currentLeader
  nodes <- view (cfg.otherNodes)
  case mlid of
    Just lid -> case Set.lookupGT lid nodes of
      Just nlid -> currentLeader .= Just nlid
      Nothing   -> setLeaderToFirst
    Nothing -> setLeaderToFirst

clientSendCommand :: Command nt et -> Raft nt et rt mt ()
clientSendCommand cmd@Command{..} = do
  mlid <- use currentLeader
  case mlid of
    Just lid -> do
      sendRPC lid $ CMD cmd
      prcount <- fmap Map.size (use pendingRequests)
      -- if this will be our only pending request, start the timer
      -- otherwise, it should already be running
      when (prcount == 0) resetHeartbeatTimer
      pendingRequests %= Map.insert _cmdRequestId cmd
    Nothing  -> do
      setLeaderToFirst
      clientSendCommand cmd

clientHandleCommandResponse :: (rt -> Raft nt et rt mt ())
                            -> CommandResponse nt rt
                            -> Raft nt et rt mt ()
clientHandleCommandResponse useResult CommandResponse{..} = do
  prs <- use pendingRequests
  when (Map.member _cmdrRequestId prs) $ do
    useResult _cmdrResult
    pendingRequests %= Map.delete _cmdrRequestId
    prcount <- fmap Map.size (use pendingRequests)
    -- if we still have pending requests, reset the timer
    -- otherwise cancel it
    if (prcount > 0)
      then resetHeartbeatTimer
      else cancelTimer
