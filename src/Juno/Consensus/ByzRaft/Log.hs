module Juno.Consensus.ByzRaft.Log
  ( addLogEntryAndHash
  , updateLogHashesFromIndex)
where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Codec.Digest.SHA
import Data.Serialize
import qualified Data.ByteString as B

import Juno.Runtime.Types
import Juno.Util.Util

-- we need the Serialize instances for hashing the log entry
import Juno.Persistence.SQLite ()
-- see Runtime.Types not on Provenance's serialize instance for details

-- TODO: This uses the old decode encode trick and should be changed...
hashLogEntry :: Maybe LogEntry -> LogEntry -> LogEntry
hashLogEntry (Just LogEntry{ _leHash = prevHash}) le =
  le { _leHash = hash SHA256 (encode (le { _leHash = prevHash }))}
hashLogEntry Nothing le =
  le { _leHash = hash SHA256 (encode (le { _leHash = B.empty }))}

updateLogHashesFromIndex :: LogIndex -> Seq LogEntry -> Seq LogEntry
updateLogHashesFromIndex i es =
  case seqIndex es $ fromIntegral i of
    Just _ -> do
      logEntries' <- return (Seq.adjust (hashLogEntry (seqIndex es (fromIntegral i - 1))) (fromIntegral i) es)
      updateLogHashesFromIndex (i + 1) logEntries'
    Nothing -> es

addLogEntryAndHash :: LogEntry -> Seq LogEntry -> Seq LogEntry
addLogEntryAndHash le es =
  case Seq.viewr es of
    _ Seq.:> ple -> es Seq.|> hashLogEntry (Just ple) le
    Seq.EmptyR   -> Seq.singleton (hashLogEntry Nothing le)
