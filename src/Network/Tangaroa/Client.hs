{-# LANGUAGE RecordWildCards #-}

module Network.Tangaroa.Client
  ( runRaftClient
  ) where

import Network.Tangaroa.Timer
import Network.Tangaroa.Types
import Network.Tangaroa.Util
import Network.Tangaroa.Sender (sendRPC)

import Control.Concurrent.Chan.Unagi
import Control.Lens hiding (Index)
import Control.Monad.RWS
import qualified Data.Set as Set

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
  messageCount .= 0
  clientHandleEvents useResult

-- alias this lens for the client's purposes
messageCount :: Lens' (RaftState nt et) Index
messageCount = commitIndex

commandGetter :: Raft nt et rt mt et -> Raft nt et rt mt ()
commandGetter getEntry = do
  nid <- view (cfg.nodeId)
  forever $ do
    entry <- getEntry
    let cmd = Command entry nid
    enqueueEvent $ ERPC $ CMD cmd

clientHandleEvents :: Ord nt => (rt -> Raft nt et rt mt ()) -> Raft nt et rt mt ()
clientHandleEvents useResult = forever $ do
  e <- dequeueEvent
  case e of
    ERPC (CMD cmd)     -> clientSendCommand cmd -- these are commands coming from the commandGetter thread
    ERPC (CMDR cmdr)   -> clientHandleCommandResponse useResult cmdr
    HeartbeatTimeout _ -> do
      debug "choosing a new leader, try resending commands"
      setLeaderToNext
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
clientSendCommand cmd = do
  mlid <- use currentLeader
  case mlid of
    Just lid -> do
      sendRPC lid $ CMD cmd
      messageCount += 1
      resetHeartbeatTimer
    Nothing  -> do
      setLeaderToFirst
      clientSendCommand cmd

clientHandleCommandResponse :: (rt -> Raft nt et rt mt ())
                            -> CommandResponse nt rt
                            -> Raft nt et rt mt ()
clientHandleCommandResponse useResult CommandResponse{..} = do
  messageCount -= 1
  mcnt <- use messageCount
  if mcnt > 0
    then resetHeartbeatTimer
    else cancelTimer
  useResult _cmdrResult
