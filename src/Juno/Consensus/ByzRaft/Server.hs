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
import qualified System.Remote.Monitoring as EKG

runRaftServer :: Config -> RaftSpec (Raft IO) -> EKG.Server -> IO ()
runRaftServer rconf spec ekgServer = do
  let qsize = getQuorumSize $ 1 + (Set.size $ rconf ^. otherNodes)
  runRWS_
    raft
    (RaftEnv rconf qsize spec ekgServer)
    initialRaftState

-- THREAD: SERVER MAIN
raft :: Raft IO ()
raft = do
  void $ CL.fork messageReceiver -- THREAD: SERVER MESSAGE RECEIVER
  resetElectionTimer
  handleEvents
