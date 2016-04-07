
module Juno.Consensus.ByzRaft.Handler
  ( handleEvents )
where

import qualified Data.ByteString as B
import Data.Sequence
import Control.Monad (forever, join)
import Control.Lens
import Data.AffineSpace

import Juno.Runtime.Types
import Juno.Consensus.ByzRaft.Commit (doCommit)
import Juno.Runtime.Sender (sendAllAppendEntries,sendAllAppendEntriesResponse)
import Juno.Util.Util (debug, dequeueEvent)
import qualified Juno.Consensus.Pure.Handle.AppendEntries as PureAppendEntries
import qualified Juno.Consensus.Pure.Handle.AppendEntriesResponse as PureAppendEntriesResponse
import qualified Juno.Consensus.Pure.Handle.RequestVote as PureRequestVote
import qualified Juno.Consensus.Pure.Handle.RequestVoteResponse as PureRequestVoteResponse
import qualified Juno.Consensus.Pure.Handle.Command as PureCommand
import qualified Juno.Consensus.Pure.Handle.ElectionTimeout as PureElectionTimeout
import qualified Juno.Consensus.Pure.Handle.HeartbeatTimeout as PureHeartbeatTimeout
import qualified Juno.Consensus.Pure.Handle.Revolution as PureRevolution


issueBatch :: Monad m => Raft m ()
issueBatch = do
  role' <- use role
  case role' of
    Follower -> return ()
    Candidate -> return ()
    Leader -> do
      -- right now, only batch if leader
      batchTimeDelta' <- view (cfg.batchTimeDelta)
      curTime <- join $ view (rs.getTimestamp)
      (ts, h) <- use lLastBatchUpdate
      if (curTime .-. ts) >= batchTimeDelta'
      -- If enough time has elapsed, then figure out if anything new has happened
      then do
        -- the main point here is to batch up a bunch of work, specifically:
        --   - dealing with AER's and their responses
        --   - auto-batching commands into a single AE
        --   - delaying the execution of evidence checking
        doCommit
        les <- use logEntries
        latestLogHash <- return $ case viewr les of
          EmptyR -> B.empty
          _ :> LogEntry _ _ h' -> h'
        if latestLogHash /= h || latestLogHash == B.empty
        then do
          sendAllAppendEntriesResponse
          sendAllAppendEntries
          curTime' <- join $ view (rs.getTimestamp)
          lLastBatchUpdate .= (curTime', h)
          debug "Batch Issuance Triggered"
        else do
          curTime' <- join $ view (rs.getTimestamp)
          lLastBatchUpdate .= (curTime', h)
      else
        return ()

-- THREAD: SERVER MAIN
handleEvents :: Monad m => Raft m ()
handleEvents = forever $ do
  e <- dequeueEvent
  case e of
    ERPC rpc           -> handleRPC rpc >> issueBatch
    AERs alotOfAers    -> PureAppendEntriesResponse.handleAlotOfAers alotOfAers >> issueBatch
    ElectionTimeout s  -> PureElectionTimeout.handle s >> issueBatch
    HeartbeatTimeout s -> PureHeartbeatTimeout.handle s >> issueBatch

handleRPC :: Monad m => RPC -> Raft m ()
handleRPC rpc = case rpc of
  AE' ae          -> PureAppendEntries.handle ae
  AER' aer        -> PureAppendEntriesResponse.handle aer
  RV' rv          -> PureRequestVote.handle rv
  RVR' rvr        -> PureRequestVoteResponse.handle rvr
  CMD' cmd        -> PureCommand.handle cmd
  CMDB' cmdb      -> PureCommand.handleBatch cmdb
  CMDR' _         -> debug "got a command response RPC"
  REV' rev        -> PureRevolution.handle rev
