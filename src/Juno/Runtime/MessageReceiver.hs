{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Juno.Runtime.MessageReceiver
    (
     messageReceiver
    ) where


import Control.Lens
import Control.Monad
import Control.Parallel.Strategies
import Data.Either (partitionEithers)
import Data.List (partition)
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import qualified Data.Set as Set

import Juno.Runtime.Types
import Juno.Util.Util (debugNoRole,enqueueEvent)


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
    unless (alotOfAers == mempty) $ do debugNoRole $ "Combined together " ++ show howManyAers ++ " AERs"
                                       enqueueEvent $ AERs alotOfAers
    mapM_ debugNoRole invalidAers
    -- sip from the general message stream, this should be relatively underpopulated except during an election but can contain HUGE AEs
    gm 50 >>= sequentialVerify ks
    -- now take a massive gulp of commands
    verifiedCmds <- parallelVerify ks <$> getCmds 5000
    (invalidCmds, validCmds) <- return $ partitionEithers verifiedCmds
    mapM_ debugNoRole invalidCmds
    cmds@(CommandBatch cmds' _) <- return $ batchCommands validCmds
    lenCmdBatch <- return $ length cmds'
    unless (lenCmdBatch == 0) $ do
      enqueueEvent $ ERPC $ CMDB' cmds
      debugNoRole $ "AutoBatched " ++ show (length cmds') ++ " Commands"


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
  mapM_ debugNoRole invalid
  -- AE's have the potential to be BIG so we need to take care not to do them in parallel by accident
  mapM_ (\(ts,msg) -> case signedRPCtoRPC (Just ts) ks msg of
            Left err -> debugNoRole err
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


aerOnlyDecode :: ReceivedAt -> SignedRPC -> Either String AppendEntriesResponse
aerOnlyDecode ts s@SignedRPC{..}
  | _digType _sigDigest /= AER = error $ "Invariant Error: aerOnlyDecode called on " ++ show s
  | otherwise = case S.decode _sigBody of
      Left !err -> Left $! "Failure to decode AERWire: " ++ err
      Right (AERWire !(t,nid,s',c,i,h)) -> Right $! AppendEntriesResponse t nid s' c i h False $ ReceivedMsg _sigDigest _sigBody $ Just ts
