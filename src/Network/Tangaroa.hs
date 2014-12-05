{-# LANGUAGE RecordWildCards #-}

module Network.Tangaroa
  ( runRaft
  , RaftSpec(..)
  , Config(..), otherNodes, nodeId, electionTimeoutRange, heartbeatTimeout, enableDebug
  , Term, startTerm
  ) where

import Control.Concurrent.Chan.Unagi
import Control.Lens
import qualified Data.Set as Set

import Network.Tangaroa.Handler
import Network.Tangaroa.Types
import Network.Tangaroa.Util
import Network.Tangaroa.Timer

runRaft :: Ord nt => Config nt -> RaftSpec nt et rt mt -> IO ()
runRaft rconf spec@RaftSpec{..} = do
  let qsize = getQuorumSize $ 1 + (Set.size $ rconf ^. otherNodes)
  (ein, eout) <- newChan
  runRWS_
    raft
    (RaftEnv rconf qsize ein eout (liftRaftSpec spec))
    initialRaftState

raft :: Ord nt => Raft nt et rt mt ()
raft = do
  fork_ messageReceiver
  resetElectionTimer
  handleEvents
