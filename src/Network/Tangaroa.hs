{-# LANGUAGE RecordWildCards #-}

module Network.Tangaroa
  ( runRaft
  , module Network.Tangaroa.Types
  ) where

import Control.Concurrent.Chan.Unagi
import Control.Lens hiding (Index)
import Control.Monad
import Control.Monad.RWS
import qualified Data.Set as Set

import Network.Tangaroa.Types
import Network.Tangaroa.Internal.Monad

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
  forever $ gm >>= enqueueEvent . Message
