module Juno.Consensus.Server
  ( runRaftServer
  ) where

import Control.Lens
import qualified Data.Set as Set

import Juno.Consensus.Handle
import Juno.Consensus.Api (apiReceiver)
import Juno.Types
import Juno.Util.Util
import Juno.Runtime.Timer
import Juno.Runtime.MessageReceiver

import qualified Control.Concurrent.Lifted as CL
import Control.Monad

runRaftServer :: ReceiverEnv -> Config -> RaftSpec (Raft IO) -> IO ()
runRaftServer renv rconf spec = do
  let csize = 1 + Set.size (rconf ^. otherNodes)
      qsize = getQuorumSize csize
  void $ runMessageReceiver renv
  runRWS_
    raft
    (RaftEnv rconf csize qsize spec)
    initialRaftState

-- THREAD: SERVER MAIN
raft :: Raft IO ()
raft = do
  logStaticMetrics
  void $ CL.fork apiReceiver     -- THREAD: waits for cmds from API, signs and sends to leader.
  resetElectionTimer
  handleEvents
