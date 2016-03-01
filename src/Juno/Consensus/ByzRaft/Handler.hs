
module Juno.Consensus.ByzRaft.Handler
  ( handleEvents )
where

import Control.Monad hiding (mapM)
import Prelude hiding (mapM, all)

import Juno.Runtime.Types
import Juno.Util.Util
import qualified Juno.Consensus.Pure.Handle.AppendEntries as PureAppendEntries
import qualified Juno.Consensus.Pure.Handle.AppendEntriesResponse as PureAppendEntriesResponse
import qualified Juno.Consensus.Pure.Handle.RequestVote as PureRequestVote
import qualified Juno.Consensus.Pure.Handle.RequestVoteResponse as PureRequestVoteResponse
import qualified Juno.Consensus.Pure.Handle.Command as PureCommand
import qualified Juno.Consensus.Pure.Handle.ElectionTimeout as PureElectionTimeout
import qualified Juno.Consensus.Pure.Handle.HeartbeatTimeout as PureHeartbeatTimeout
import qualified Juno.Consensus.Pure.Handle.Revolution as PureRevolution


-- THREAD: SERVER MAIN
handleEvents :: Monad m => Raft m ()
handleEvents = forever $ do
  e <- dequeueEvent
  case e of
    ERPC rpc           -> handleRPC rpc
    ElectionTimeout s  -> PureElectionTimeout.handle s
    HeartbeatTimeout s -> PureHeartbeatTimeout.handle s

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb ma = do
  b <- mb
  when b ma

handleRPC :: Monad m => RPC -> Raft m ()
handleRPC rpc = case rpc of
  AE ae          -> whenM (verifyRPCWithKey rpc) $ PureAppendEntries.handle ae
  AER aer        -> whenM (verifyRPCWithKey rpc) $ PureAppendEntriesResponse.handle aer
  RV rv          -> whenM (verifyRPCWithKey rpc) $ PureRequestVote.handle rv
  RVR rvr        -> whenM (verifyRPCWithKey rpc) $ PureRequestVoteResponse.handle rvr
  CMD cmd        -> whenM (verifyRPCWithClientKey rpc) $ PureCommand.handle cmd
  CMDR _         -> whenM (verifyRPCWithKey rpc) $ debug "got a command response RPC"
  DBG s          -> debug $ "got a debug RPC: " ++ s
  REVOLUTION rev -> whenM (verifyRPCWithClientKey rpc) $ PureRevolution.handle rev
