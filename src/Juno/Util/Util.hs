{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Juno.Util.Util
  ( seqIndex
  , lastLogInfo
  , getQuorumSize
  , debug
  , randomRIO
  , runRWS_
  , enqueueEvent, enqueueEventLater
  , dequeueEvent
  , logMetric
  , logStaticMetrics
  , messageReceiver
  , setTerm
  , setRole
  , setCurrentLeader
  , updateLNextIndex
  , setLNextIndex
  , getCmdSigOrInvariantError
  , getRevSigOrInvariantError
  ) where

import Juno.Runtime.Types
import Juno.Util.Combinator

import Data.Either (partitionEithers)
import Data.List (partition)
import Control.Lens
import Control.Parallel.Strategies
import Data.Sequence (Seq)
import Control.Monad.RWS
import Data.ByteString (ByteString)

import qualified Control.Concurrent.Lifted as CL
import qualified Data.ByteString as B
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified System.Random as R

seqIndex :: Seq a -> Int -> Maybe a
seqIndex s i =
  if i >= 0 && i < Seq.length s
    then Just (Seq.index s i)
    else Nothing

getQuorumSize :: Int -> Int
getQuorumSize n = minimum [n - f | f <- [0..n], n >= 3*f + 1]

-- get the last term and index of a log
lastLogInfo :: Seq LogEntry -> (Term, LogIndex, ByteString)
lastLogInfo es =
  case Seq.viewr es of                 -- \/ TODO: This smells weird, should we really use length for this?
    _ Seq.:> LogEntry{..} -> (_leTerm, LogIndex $ Seq.length es - 1, _leHash)
    Seq.EmptyR            -> (startTerm, startIndex, B.empty)

debug :: Monad m => String -> Raft m ()
debug s = do
  dbg <- view (rs.debugPrint)
  nid <- view (cfg.nodeId)
  role' <- use role
  let prettyRole = case role' of
        Leader -> "\ESC[0;34m[LEADER]\ESC[0m"
        Follower -> "\ESC[0;32m[FOLLOWER]\ESC[0m"
        Candidate -> "\ESC[1;33m[CANDIDATE]\ESC[0m"
  dbg nid $ prettyRole ++ ": " ++ s

randomRIO :: (Monad m, R.Random a) => (a,a) -> Raft m a
randomRIO rng = view (rs.random) >>= \f -> f rng -- R.randomRIO

runRWS_ :: Monad m => RWST r w s m a -> r -> s -> m ()
runRWS_ ma r s = runRWST ma r s >> return ()

-- no state update
enqueueEvent :: Monad m => Event -> Raft m ()
enqueueEvent event = view (rs.enqueue) >>= \f -> f event
  -- lift $ writeChan ein event

enqueueEventLater :: Monad m => Int -> Event -> Raft m CL.ThreadId
enqueueEventLater t event = view (rs.enqueueLater) >>= \f -> f t event

-- no state update
dequeueEvent :: Monad m => Raft m Event
dequeueEvent = join $ view (rs.dequeue)

logMetric :: Monad m => Metric -> Raft m ()
logMetric metric = view (rs.publishMetric) >>= \f -> f metric

logStaticMetrics :: Monad m => Raft m ()
logStaticMetrics = do
  logMetric . MetricNodeId =<< view (cfg.nodeId)
  logMetric . MetricClusterSize =<< view clusterSize
  logMetric . MetricQuorumSize =<< view quorumSize

-- | Thread to take incoming messages and write them to the event queue.
-- THREAD: MESSAGE RECEIVER (client and server), no state updates
messageReceiver :: Monad m => Raft m ()
messageReceiver = do
  gm <- view (rs.getMessages)
  getCmds <- view (rs.getNewCommands)
  getAers <- view (rs.getNewEvidence)
  ks <- KeySet <$> view (cfg.publicKeys) <*> view (cfg.clientPublicKeys)
  forever $ do
    -- NB: This all happens on one thread because it runs in Raft and we're trying (too hard) to avoid running in IO

    -- Take a big gulp of AERs, the more we get the more we can skip
    (howManyAers, alotOfAers, invalidAers) <- toAlotOfAers <$> getAers 2000
    unless (alotOfAers == mempty) $ do debug $ "Smashed together " ++ show howManyAers ++ " AERs"
                                       enqueueEvent $ AERs alotOfAers
    mapM_ debug invalidAers
    -- sip from the general message stream, this should be relatively underpopulated except during an election but can contain HUGE AEs
    gm 50 >>= sequentialVerify ks
    -- now take a massive gulp of commands
    verifiedCmds <- parallelVerify ks <$> getCmds 5000
    (invalidCmds, validCmds) <- return $ partitionEithers verifiedCmds
    mapM_ debug invalidCmds
    cmds@(CommandBatch cmds' _) <- return $ batchCommands validCmds
    lenCmdBatch <- return $ length cmds'
    unless (lenCmdBatch == 0) $ do
      enqueueEvent $ ERPC $ CMDB' cmds
      debug $ "AutoBatched " ++ show (length cmds') ++ " Commands"

toAlotOfAers :: [(ReceivedAt,SignedRPC)] -> (Int, AlotOfAERs, [String])
toAlotOfAers s = (length decodedAers, alotOfAers, invalids)
  where
    (invalids, decodedAers) = partitionEithers $ uncurry aerOnlyDecode <$> s
    mkAlot aer@AppendEntriesResponse{..} = AlotOfAERs $ Map.insert _aerNodeId (Set.singleton aer) Map.empty
    alotOfAers = mconcat (mkAlot <$> decodedAers)

sequentialVerify :: Monad m => KeySet -> [(ReceivedAt, SignedRPC)] -> Raft m ()
sequentialVerify ks msgs = do
  (aes, noAes) <- return $ partition (\(_,SignedRPC{..}) -> if _digType _sigDigest == AE then True else False) msgs
  (invalid, validNoAes) <- return $ partitionEithers $ parallelVerify ks noAes
  mapM_ (enqueueEvent . ERPC) validNoAes
  mapM_ debug invalid
  -- AE's have the potential to be BIG so we need to take care not to do them in parallel by accident
  mapM_ (\(ts,msg) -> case signedRPCtoRPC (Just ts) ks msg of
            Left err -> debug err
            Right v -> enqueueEvent $ ERPC v) aes

parallelVerify :: KeySet -> [(ReceivedAt, SignedRPC)] -> [Either String RPC]
parallelVerify ks msgs = ((\(ts, msg) -> signedRPCtoRPC (Just ts) ks msg) <$> msgs) `using` parList rseq

batchCommands :: [RPC] -> CommandBatch
batchCommands cmdRPCs = cmdBatch
  where
    cmdBatch = CommandBatch (concat (prepCmds <$> cmdRPCs)) NewMsg
    prepCmds (CMD' cmd) = [cmd]
    prepCmds (CMDB' (CommandBatch cmds _)) = cmds
    prepCmds o = error $ "Invariant failure in batchCommands: " ++ show o

setTerm :: Monad m => Term -> Raft m ()
setTerm t = do
  void $ rs.writeTermNumber ^$ t
  term .= t
  logMetric $ MetricTerm t

setRole :: Monad m => Role -> Raft m ()
setRole newRole = do
  role .= newRole
  logMetric $ MetricRole newRole

setCurrentLeader :: Monad m => Maybe NodeID -> Raft m ()
setCurrentLeader mNode = do
  currentLeader .= mNode
  logMetric $ MetricCurrentLeader mNode

updateLNextIndex :: Monad m
                 => (Map.Map NodeID LogIndex -> Map.Map NodeID LogIndex)
                 -> Raft m ()
updateLNextIndex f = do
  lNextIndex %= f
  lni <- use lNextIndex
  ci <- use commitIndex
  logMetric $ MetricAvailableSize $ availSize lni ci

  where
    -- | The number of nodes at most one behind the commit index
    availSize lni ci = let oneBehind = pred ci
                       in succ $ Map.size $ Map.filter (>= oneBehind) lni

setLNextIndex :: Monad m
              => Map.Map NodeID LogIndex
              -> Raft m ()
setLNextIndex = updateLNextIndex . const

getCmdSigOrInvariantError :: String -> Command -> Signature
getCmdSigOrInvariantError where' s@Command{..} = case _cmdProvenance of
  NewMsg -> error $ where'
    ++ ": This should be unreachable, somehow an AE got through with a LogEntry that contained an unsigned Command" ++ show s
  ReceivedMsg{..} -> _digSig _pDig

getRevSigOrInvariantError :: String -> Revolution -> Signature
getRevSigOrInvariantError where' s@Revolution{..} = case _revProvenance of
  NewMsg -> error $ where'
    ++ ": This should be unreachable, got an unsigned Revolution" ++ show s
  ReceivedMsg{..} -> _digSig _pDig
