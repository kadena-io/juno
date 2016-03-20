{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Juno.Runtime.Sender
  ( sendAppendEntries
  , sendAppendEntriesResponse
  , createRequestVoteResponse
  , sendAllAppendEntries
  , sendAllAppendEntriesResponse
  , sendResults
  , sendRPC
  ) where

import Control.Lens
import Control.Monad.Writer
import Data.Foldable (traverse_)
import Data.Sequence (Seq)
import Data.Set (Set)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Serialize

import Juno.Util.Util
import Juno.Runtime.Types


-- TODO: There seems to be needless construction then destruction of the non-wire message types
--       Not sure if that could impact performance or if it will be unrolled/magic-ified
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
  sendRPC target $ AE' $
    AppendEntries ct nid pli plt (Seq.drop (fromIntegral $ pli + 1) es) qVoteList NewMsg

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
  debug $ "Sent AppendEntriesResponse: " ++ show ct
  (_, lindex, lhash) <- lastLogInfo <$> use logEntries
  sendRPC target $ AER' $ AppendEntriesResponse ct nid success convinced lindex lhash NewMsg

-- no state update but uses state
sendAllAppendEntriesResponse :: Monad m => Raft m ()
sendAllAppendEntriesResponse =
  traverse_ (\n -> sendAppendEntriesResponse n True True) =<< view (cfg.otherNodes)

createRequestVoteResponse :: MonadWriter [String] m => Term -> LogIndex -> NodeID -> NodeID -> Bool -> m RequestVoteResponse
createRequestVoteResponse term' logIndex' myNodeId' target vote = do
  tell ["Created RequestVoteResponse: " ++ show term']
  return $ RequestVoteResponse term' logIndex' myNodeId' vote target NewMsg

-- no state update
sendAllAppendEntries :: Monad m => Raft m ()
sendAllAppendEntries = traverse_ sendAppendEntries =<< view (cfg.otherNodes)

-- no state update
sendResults :: Monad m => Seq (NodeID, CommandResponse) -> Raft m ()
sendResults results = traverse_ (\(target,cmdr) -> sendRPC target $ CMDR' cmdr) results

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


-- TODO: figure out if there is a needless performance hit here (looking up these constants every time?)
sendRPC :: Monad m => NodeID -> RPC -> Raft m ()
sendRPC target rpc = do
  send <- view (rs.sendMessage)
  myNodeId <- view (cfg.nodeId)
  privKey <- view (cfg.myPrivateKey)
  pubKey <- view (cfg.myPublicKey)
  send target $ encode $ rpcToSignedRPC myNodeId pubKey privKey rpc
