{-# LANGUAGE RecordWildCards #-}

module Juno.Consensus.ByzRaft.Commit
  (doCommit
  ,makeCommandResponse
  ,makeCommandResponse')
where

import Control.Lens
import Control.Monad
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Control.Monad.Loops (allM)

import Juno.Runtime.Types
import Juno.Util.Util
import Juno.Runtime.Sender (sendResults)


-- THREAD: SERVER MAIN.
doCommit :: Monad m => Raft m ()
doCommit = do
  commitUpdate <- updateCommitIndex
  when commitUpdate applyLogEntries

-- apply the un-applied log entries up through commitIndex
-- and send results to the client if you are the leader
-- TODO: have this done on a separate thread via event passing
-- THREAD: SERVER MAIN. updates state
applyLogEntries :: Monad m => Raft m ()
applyLogEntries = do
  la <- use lastApplied
  ci <- use commitIndex
  le <- use logEntries
  let leToApply = Seq.drop (fromIntegral $ la + 1) . Seq.take (fromIntegral $ ci + 1) $ le
  results <- mapM (applyCommand . _leCommand) leToApply
  r <- use role
  when (r == Leader) $ sendResults results
  lastApplied .= ci


applyCommand :: Monad m => Command -> Raft m (NodeID, CommandResponse)
applyCommand cmd@Command{..} = do
  apply <- view (rs.applyLogEntry)
  result <- apply _cmdEntry
  replayMap %= Map.insert (_cmdClientId, _cmdSig) (Just result)
  ((,) _cmdClientId) <$> makeCommandResponse cmd result

makeCommandResponse :: Monad m => Command -> CommandResult -> Raft m CommandResponse
makeCommandResponse cmd result = do
  nid <- view (cfg.nodeId)
  mlid <- use currentLeader
  return $ makeCommandResponse' nid mlid cmd result

makeCommandResponse' :: NodeID -> Maybe NodeID -> Command -> CommandResult -> CommandResponse
makeCommandResponse' nid mlid Command{..} result = CommandResponse
             result
             (maybe nid id mlid)
             nid
             _cmdRequestId
             LB.empty



-- checks to see what the largest N where a quorum of nodes
-- has sent us proof of a commit up to that index
-- THREAD: SERVER MAIN. updates state
updateCommitIndex :: Monad m => Raft m Bool
updateCommitIndex = do
  ci <- use commitIndex
  proof <- use commitProof
  qsize <- view quorumSize
  es <- use logEntries

  -- get all indices in the log past commitIndex
  -- TODO: look into the overloading of LogIndex w.r.t. Seq Length/entry location
  let inds = [(ci + 1)..(fromIntegral $ Seq.length es - 1)]

  -- get the prefix of these indices where a quorum of nodes have
  -- provided proof of having replicated that entry
  let qcinds = takeWhile (\i -> (not . Map.null) (Map.filterWithKey (\k s -> k >= i && Set.size s + 1 >= qsize) proof)) inds

  case qcinds of
    [] -> return False
    _  -> do
      let qci = last qcinds
      case Map.lookup qci proof of
        Just s -> do
          let lhash = _leHash (Seq.index es $ fromIntegral qci)
          valid <- checkCommitProof qci lhash s
          if valid
            then do
              commitIndex .= qci
              commitProof %= Map.filterWithKey (\k _ -> k >= qci)
              debug $ "commit index is now: " ++ show qci
              return True
            else
              return False
        Nothing -> return False

checkCommitProof :: Monad m => LogIndex -> B.ByteString -> Set.Set AppendEntriesResponse -> Raft m Bool
checkCommitProof ci lhash aers = do
  sigsOkay <- allM (verifyRPCWithKey . AER) (Set.toList aers)
  return $ sigsOkay && all (\AppendEntriesResponse{..} -> _aerHash == lhash && _aerIndex == ci) aers
