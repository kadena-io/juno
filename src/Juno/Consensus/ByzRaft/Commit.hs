{-# LANGUAGE RecordWildCards #-}

module Juno.Consensus.ByzRaft.Commit
  (doCommit
  ,makeCommandResponse
  ,makeCommandResponse')
where

import Data.List
import Control.Lens
import Control.Monad
import Data.AffineSpace ((.-.))
import Data.Int (Int64)
import Data.Thyme.Clock (UTCTime, microseconds)

import qualified Data.Sequence as Seq
import qualified Data.Map as Map

import Data.Foldable (toList)

import Juno.Runtime.Types hiding (valid)
import Juno.Util.Util
import Juno.Runtime.Sender (sendResults)
import Juno.Runtime.Log

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
  let leToApply = Seq.drop (fromIntegral $ la + 1) . takeEntries (ci + 1) $ le
  results <- mapM (applyCommand . _leCommand) leToApply
  r <- use role
  lastApplied .= ci
  logMetric $ MetricAppliedIndex ci
  if not (null results)
    then if r == Leader
         then do
           debug $ "Applied and Responded to " ++ show (length results) ++ " CMD(s)"
           sendResults $! toList results
         else debug $ "Applied " ++ show (length results) ++ " CMD(s)"
    else debug "Applied log entries but did not send results?"

interval :: UTCTime -> UTCTime -> Int64
interval start end = view microseconds $ end .-. start

logApplyLatency :: Monad m => Command -> Raft m ()
logApplyLatency (Command _ _ _ provenance) = case provenance of
  NewMsg -> return ()
  ReceivedMsg _digest _orig mReceivedAt -> case mReceivedAt of
    Just (ReceivedAt arrived) -> do
      now <- join $ view (rs.getTimestamp)
      logMetric $ MetricApplyLatency $ fromIntegral $ interval arrived now
    Nothing -> return ()

applyCommand :: Monad m => Command -> Raft m (NodeID, CommandResponse)
applyCommand cmd@Command{..} = do
  apply <- view (rs.applyLogEntry)
  logApplyLatency cmd
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
          let duration = interval lastTime now
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

  -- get the bound for things we can deal with
  -- TODO: look into the overloading of LogIndex w.r.t. Seq Length/entry location
  let maxLogIndex = maxIndex es

  -- this gets us all of the evidence we have, in order of largest LogIndex to smallest
  let evidence = reverse $ sortOn _aerIndex $ Map.elems proof

  case checkCommitProof qsize es maxLogIndex evidence of
    Left 0 -> return False
    Left n -> if maxLogIndex > fromIntegral ci
              then do
                    debug $ "Not enough evidence to commit yet, need " ++ show (qsize - n) ++ " more"
                    return False
              else return False
    Right qci -> if qci > ci
                then do
                  commitIndex .= qci
                  logCommitChange ci qci
                  commitProof %= Map.filter (\a -> qci < _aerIndex a)
                  debug $ "Commit index is now: " ++ show qci
                  return True
                else return False

checkCommitProof :: Int -> Log LogEntry -> LogIndex -> [AppendEntriesResponse] -> Either Int LogIndex
checkCommitProof qsize les maxLogIdx evidence = go 0 evidence
  where
    go n [] = Left n -- no update
    go n (ev:evs) = if _aerIndex ev > maxLogIdx
                    -- we can't do the lookup as we haven't replicated the entry yet, so pass till next time
                    then go n evs
                    else if Just (_aerHash ev) == (_leHash <$> lookupEntry (_aerIndex ev) les)
                         -- hashes check out, if we have enough evidence then we can commit
                         then if (n+1) >= qsize
                              then Right $ _aerIndex ev
                              -- keep checking the evidence
                              else go (n+1) evs
                         -- hash check failed, can't count this one...
                         else go n evs
