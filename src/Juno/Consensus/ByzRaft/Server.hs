module Juno.Consensus.ByzRaft.Server
  ( runRaftServer
  ) where

import Control.Lens
import qualified Data.Set as Set

import Juno.Consensus.ByzRaft.Handler
import Juno.Runtime.Types
import Juno.Util.Util
import Juno.Runtime.Timer

import qualified Control.Concurrent.Lifted as CL
import Control.Monad

runRaftServer :: Config -> RaftSpec (Raft IO) -> IO ()
runRaftServer rconf spec = do
  let csize = 1 + (Set.size $ rconf ^. otherNodes)
      qsize = getQuorumSize csize
  runRWS_
    raft
    (RaftEnv rconf csize qsize spec)
    initialRaftState

-- THREAD: SERVER MAIN
raft :: Raft IO ()
raft = do
  void $ CL.fork messageReceiver -- THREAD: SERVER MESSAGE RECEIVER
  resetElectionTimer
  handleEvents
