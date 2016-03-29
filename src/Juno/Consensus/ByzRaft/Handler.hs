
module Juno.Consensus.ByzRaft.Handler
  ( handleEvents )
where

import Control.Monad (forever)

import Juno.Runtime.Types
import Juno.Util.Util (debug, dequeueEvent)
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

handleRPC :: Monad m => RPC -> Raft m ()
handleRPC rpc = case rpc of
  AE' ae          -> PureAppendEntries.handle ae
  AER' aer        -> PureAppendEntriesResponse.handle aer
  RV' rv          -> PureRequestVote.handle rv
  RVR' rvr        -> PureRequestVoteResponse.handle rvr
  CMD' cmd        -> PureCommand.handle cmd
  CMDB' cmdb      -> debug "Got a CommandBatch" >> PureCommand.handleBatch cmdb >> debug "Completed CommandBatch Processing"
  CMDR' _         -> debug "got a command response RPC"
  REV' rev        -> PureRevolution.handle rev
