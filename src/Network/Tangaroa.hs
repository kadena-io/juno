{-# LANGUAGE RecordWildCards #-}

module Network.Tangaroa
  ( runRaft
  , RaftSpec(..)
  , Config(..), otherNodes, nodeId, electionTimeoutRange, heartbeatTimeout
  , Term, startTerm
  ) where

import Control.Concurrent.Chan.Unagi
import Control.Lens hiding (Index)
import Control.Monad
import Control.Monad.RWS
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Map as Map

import Network.Tangaroa.Handler
import Network.Tangaroa.Types
import Network.Tangaroa.Util
import Network.Tangaroa.Timer

getQsize :: Int -> Int
getQsize n =
  if even n
    then n `div` 2 + 1
    else (n - 1) `div` 2 + 1

runRaft :: Ord nt => Config nt -> RaftSpec nt et rt mt -> IO ()
runRaft rconf spec@RaftSpec{..} = do
  let qsize = getQsize $ 1 + (Set.size $ rconf ^. otherNodes)
  (ein, eout) <- newChan
  runRWS_
    raft
    (RaftEnv rconf qsize ein eout (liftRaftSpec spec))
    initialRaftState

initialRaftState :: RaftState nt et
initialRaftState = RaftState
  Follower   -- role
  startTerm  -- term
  Nothing    -- votedFor
  Nothing    -- currentLeader
  Seq.empty  -- log
  startIndex -- commitIndex
  startIndex -- lastApplied
  Nothing    -- timerThread
  Set.empty  -- cYesVotes
  Set.empty  -- cPotentialVotes
  Map.empty  -- lNextIndex
  Map.empty  -- lMatchIndex

runRWS_ :: Monad m => RWST r w s m a -> r -> s -> m ()
runRWS_ ma r s = runRWST ma r s >> return ()

raft :: Ord nt => Raft nt et rt mt ()
raft = do
  fork_ messageReceiver
  resetElectionTimer
  handleEvents

-- | Thread to take incoming messages and write them to the event queue.
messageReceiver :: Raft nt et rt mt ()
messageReceiver = do
  gm <- view (rs.getMessage)
  deser <- view (rs.deserializeRPC)
  forever $
    gm >>= maybe
      (debug "failed to deserialize RPC")
      (enqueueEvent . ERPC)
      . deser
