{-# LANGUAGE RecordWildCards #-}

module Juno.Consensus.ByzRaft.Log
  ( addLogEntryAndHash
  , updateLogHashesFromIndex
  , updateLogHashesFromIndex')
where

import Control.Lens
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Codec.Digest.SHA
import Data.Serialize
import qualified Data.ByteString as B

import Juno.Runtime.Types
import Juno.Util.Util

-- TODO: This uses the old decode encode trick and should be changed...
hashLogEntry :: Maybe LogEntry -> LogEntry -> LogEntry
hashLogEntry (Just LogEntry{ _leHash = prevHash }) le@LogEntry{..} =
  le { _leHash = hash SHA256 (encode $ LEWire (_leTerm, getCmdSignedRPC le, prevHash))}
hashLogEntry Nothing le@LogEntry{..} =
  le { _leHash = hash SHA256 (encode $ LEWire (_leTerm, getCmdSignedRPC le, B.empty))}

getCmdSignedRPC :: LogEntry -> SignedRPC
getCmdSignedRPC LogEntry{ _leCommand = Command{ _cmdProvenance = ReceivedMsg{ _pDig = dig, _pOrig = bdy }}} =
  SignedRPC dig bdy
getCmdSignedRPC LogEntry{ _leCommand = Command{ _cmdProvenance = NewMsg }} =
  error "Invariant Failure: for a command to be in a log entry, it needs to have been received!"

-- THREAD: SERVER MAIN. updates state
updateLogHashesFromIndex :: Monad m => LogIndex -> Raft m ()
updateLogHashesFromIndex i =
  logEntries %= updateLogHashesFromIndex' i
{-
  es <- use logEntries
  case seqIndex es $ fromIntegral i of
    Just _  -> do
      logEntries %= Seq.adjust (hashLogEntry (seqIndex es (fromIntegral i - 1))) (fromIntegral i)
      updateLogHashesFromIndex (i + 1)
    Nothing -> return ()
-}

-- pure version
updateLogHashesFromIndex' :: LogIndex -> Seq LogEntry -> Seq LogEntry
updateLogHashesFromIndex' i es =
  case seqIndex es $ fromIntegral i of
    Just _  -> do
      logEntries' <- return (Seq.adjust (hashLogEntry (seqIndex es (fromIntegral i - 1))) (fromIntegral i) es)
      updateLogHashesFromIndex' (i + 1) logEntries'
    Nothing -> es

addLogEntryAndHash :: LogEntry -> Seq LogEntry -> Seq LogEntry
addLogEntryAndHash le es =
  case Seq.viewr es of
    _ Seq.:> ple -> es Seq.|> hashLogEntry (Just ple) le
    Seq.EmptyR   -> Seq.singleton (hashLogEntry Nothing le)
