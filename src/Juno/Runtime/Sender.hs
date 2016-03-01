{-# LANGUAGE RecordWildCards #-}

module Juno.Runtime.Sender
  ( sendAppendEntries
  , sendAppendEntriesResponse
  , sendRequestVote
  , sendRequestVoteResponse
  , sendAllAppendEntries
  , sendAllRequestVotes
  , sendAllAppendEntriesResponse
  , sendResults
  , sendRPC
  , sendSignedRPC
  ) where

import Control.Lens
import Data.Foldable (traverse_)
import Data.Sequence (Seq)
import Data.Set (Set)
import qualified Data.ByteString.Lazy as B
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Juno.Util.Util
import Juno.Runtime.Types


-- no state update, uses state
sendAppendEntries :: Monad m => NodeID -> Raft m ()
sendAppendEntries target = do
  mni <- use $ lNextIndex.at target
  es <- use logEntries
  let (pli,plt) = logInfoForNextIndex mni es
  ct <- use term
  nid <- view (cfg.nodeId)
  debug $ "sendAppendEntries: " ++ show ct
  qVoteList <- getVotesForNode target
  sendSignedRPC target $ AE $
    AppendEntries ct nid pli plt (Seq.drop (fromIntegral $ pli + 1) es) qVoteList B.empty

getVotesForNode :: Monad m => NodeID -> Raft m (Set RequestVoteResponse)
getVotesForNode target = do
  convinced <- Set.member target <$> use lConvinced
  if convinced
    then return Set.empty
    else use cYesVotes

-- no state update but uses state
sendAppendEntriesResponse :: Monad m => NodeID -> Bool -> Bool -> Raft m ()
sendAppendEntriesResponse target success convinced = do
  ct <- use term
  nid <- view (cfg.nodeId)
  debug $ "sendAppendEntriesResponse: " ++ show ct
  (_, lindex, lhash) <- lastLogInfo <$> use logEntries
  sendSignedRPC target $ AER $ AppendEntriesResponse ct nid success convinced lindex lhash B.empty

-- no state update but uses state
sendAllAppendEntriesResponse :: Monad m => Raft m ()
sendAllAppendEntriesResponse =
  traverse_ (\n -> sendAppendEntriesResponse n True True) =<< view (cfg.otherNodes)

-- uses state, but does not update
sendRequestVote :: Monad m => NodeID -> Raft m ()
sendRequestVote target = do
  ct <- use term
  nid <- view (cfg.nodeId)
  (llt, lli, _) <- lastLogInfo <$> use logEntries
  debug $ "sendRequestVote: " ++ show ct
  sendSignedRPC target $ RV $ RequestVote ct nid lli llt B.empty

sendRequestVoteResponse :: Monad m => NodeID -> Bool -> Raft m ()
sendRequestVoteResponse target vote = do
  ct <- use term
  nid <- view (cfg.nodeId)
  debug $ "sendRequestVoteResponse: " ++ show ct
  sendSignedRPC target $ RVR $ RequestVoteResponse ct nid vote target B.empty

-- no state update
sendAllAppendEntries :: Monad m => Raft m ()
sendAllAppendEntries = traverse_ sendAppendEntries =<< view (cfg.otherNodes)

-- uses state, but does not update
sendAllRequestVotes :: Monad m => Raft m ()
sendAllRequestVotes = traverse_ sendRequestVote =<< use cPotentialVotes

-- no state update
sendResults :: Monad m => Seq (NodeID, CommandResponse) -> Raft m ()
sendResults results = traverse_ (\(target,cmdr) -> sendSignedRPC target $ CMDR cmdr) results

-- called by leaders sending appendEntries.
-- given a replica's nextIndex, get the index and term to send as
-- prevLog(Index/Term)
logInfoForNextIndex :: Maybe LogIndex -> Seq LogEntry -> (LogIndex,Term)
logInfoForNextIndex mni es =
  case mni of
    Just ni -> let pli = ni - 1 in
      case seqIndex es $ fromIntegral pli of
        Just LogEntry{..} -> (pli, _leTerm)
         -- this shouldn't happen, because nextIndex - 1 should always be at
         -- most our last entry
        Nothing -> (startIndex, startTerm)
    Nothing -> (startIndex, startTerm)

sendRPC :: Monad m => NodeID -> RPC -> Raft m ()
sendRPC target rpc = do
  send <- view (rs.sendMessage)
  ser <- view (rs.serializeRPC)
  send target $ ser rpc

-- no state update
sendSignedRPC :: Monad m => NodeID -> RPC -> Raft m ()
sendSignedRPC target rpc = do
  pk <- view (cfg.privateKey)
  msg <- return $ case rpc of
    AE ae          -> AE   $ signRPC pk ae
    AER aer        -> AER  $ signRPC pk aer
    RV rv          -> RV   $ signRPC pk rv
    RVR rvr        -> RVR  $ signRPC pk rvr
    CMD cmd        -> CMD  $ signRPC pk cmd
    CMDR cmdr      -> CMDR $ signRPC pk cmdr
    REVOLUTION rev -> REVOLUTION $ signRPC pk rev
    _         -> rpc
  sendRPC target msg
