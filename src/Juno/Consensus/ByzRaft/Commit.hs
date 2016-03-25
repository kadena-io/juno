{-# LANGUAGE RecordWildCards #-}

module Juno.Consensus.ByzRaft.Commit
  (doCommit
  ,makeCommandResponse
  ,makeCommandResponse')
where

import Control.Lens
import Control.Monad
import Data.AffineSpace ((.-.))
import Data.Thyme.Clock (microseconds)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import qualified Data.ByteString as B

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
  logMetric $ MetricAppliedIndex ci
  if length results > 0
    then debug $ "Sent " ++ show (length results) ++ " CMDRs"
    else debug $ "Applied log entries but did not send results?"

applyCommand :: Monad m => Command -> Raft m (NodeID, CommandResponse)
applyCommand cmd@Command{..} = do
  apply <- view (rs.applyLogEntry)
  result <- apply _cmdEntry
  replayMap %= Map.insert (_cmdClientId, getCmdSigOrInvariantError "applyCommand" cmd) (Just result)
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
             NewMsg

logCommitChange :: Monad m => LogIndex -> LogIndex -> Raft m ()
logCommitChange before after
  | after > before = do
      logMetric $ MetricCommitIndex after
      mLastTime <- use lastCommitTime
      now <- join $ view (rs.getTimestamp)
      case mLastTime of
        Nothing -> return ()
        Just lastTime ->
          let duration = view microseconds $ now .-. lastTime
              (LogIndex numCommits) = after - before
              period = fromIntegral duration / fromIntegral numCommits
          in logMetric $ MetricCommitPeriod period
      lastCommitTime ?= now
  | otherwise = return ()

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
    [] -> do
      debug "No new commit proof to check"
      return False
    _  -> do
      let qci = last qcinds
      debug $ "checking commit proof for: " ++ show qci
      case Map.lookup qci proof of
        Just s -> do
          let lhash = _leHash (Seq.index es $ fromIntegral qci)
          valid <- return $ checkCommitProof qci lhash s
          if valid
            then do
              commitIndex .= qci
              logCommitChange ci qci
              commitProof %= Map.filterWithKey (\k _ -> k >= qci)
              debug $ "Commit index is now: " ++ show qci
              return True
            else do
              debug $ "Invalid Commit Proof found for " ++ show qci ++ " and " ++ show lhash
              debug $ "Evidence was: " ++ show ((\a -> (_aerIndex a, _aerHash a)) <$> Set.toList s)
              return False
        Nothing -> do
          debug $ "No proof found at: " ++ show qci
          return False

checkCommitProof :: LogIndex -> B.ByteString -> Set.Set AppendEntriesResponse -> Bool
checkCommitProof ci lhash aers = all (\AppendEntriesResponse{..} -> _aerHash == lhash && _aerIndex == ci) aers
