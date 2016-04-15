{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Juno.Runtime.Sender
  ( sendAppendEntries
  , sendAppendEntriesResponse
  , createRequestVoteResponse
  , sendAllAppendEntries
  , sendAllAppendEntriesResponse
  , createAppendEntriesResponse
  , sendResults
  , sendRPC
  ) where

import Control.Lens
import Control.Arrow (second)
import Control.Parallel.Strategies
import Control.Monad.Writer

import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Serialize

import Juno.Util.Util
import Juno.Runtime.Types
import Juno.Runtime.Timer (resetLastBatchUpdate)
import Juno.Runtime.Ledger

createAppendEntries' :: NodeID
                   -> Map NodeID LogIndex
                   -> Ledger LogEntry
                   -> Term
                   -> NodeID
                   -> Set NodeID
                   -> Set RequestVoteResponse
                   -> RPC
createAppendEntries' target lNextIndex' es ct nid vts yesVotes =
  let
    mni = Map.lookup target lNextIndex'
    (pli,plt) = logInfoForNextIndex mni es
    vts' = if Set.member target vts then Set.empty else yesVotes
  in
    -- If we send too big of an AppendEntries we can lock their system...
    AE' $ AppendEntries ct nid pli plt (getEntriesAfter pli es) vts' NewMsg

-- TODO: There seems to be needless construction then destruction of the non-wire message types
--       Not sure if that could impact performance or if it will be unrolled/magic-ified
-- no state update, uses state
sendAppendEntries :: Monad m => NodeID -> Raft m ()
sendAppendEntries target = do
  lNextIndex' <- use lNextIndex
  es <- use logEntries
  ct <- use term
  nid <- view (cfg.nodeId)
  vts <- use lConvinced
  yesVotes <- use cYesVotes
  sendRPC target $ createAppendEntries' target lNextIndex' es ct nid vts yesVotes
  resetLastBatchUpdate
  debug $ "sendAppendEntries: " ++ show ct

-- no state update
sendAllAppendEntries :: Monad m => Raft m ()
sendAllAppendEntries = do
  lNextIndex' <- use lNextIndex
  es <- use logEntries
  ct <- use term
  nid <- view (cfg.nodeId)
  vts <- use lConvinced
  yesVotes <- use cYesVotes
  oNodes <- view (cfg.otherNodes)
  sendRPCs $ (\target -> (target, createAppendEntries' target lNextIndex' es ct nid vts yesVotes)) <$> Set.toList oNodes
  resetLastBatchUpdate
  debug "Sent All AppendEntries"

createAppendEntriesResponse' :: Bool -> Bool -> Term -> NodeID -> LogIndex -> ByteString -> RPC
createAppendEntriesResponse' success convinced ct nid lindex lhash =
  AER' $ AppendEntriesResponse ct nid success convinced lindex lhash True NewMsg

createAppendEntriesResponse :: Monad m => Bool -> Bool -> Raft m AppendEntriesResponse
createAppendEntriesResponse success convinced = do
  ct <- use term
  nid <- view (cfg.nodeId)
  (_, lindex, lhash) <- lastLogInfo <$> use logEntries
  case createAppendEntriesResponse' success convinced ct nid lindex lhash of
    AER' aer -> return aer
    _ -> error "deep invariant error"

-- no state update but uses state
sendAppendEntriesResponse :: Monad m => NodeID -> Bool -> Bool -> Raft m ()
sendAppendEntriesResponse target success convinced = do
  ct <- use term
  nid <- view (cfg.nodeId)
  (_, lindex, lhash) <- lastLogInfo <$> use logEntries
  sendRPC target $ createAppendEntriesResponse' success convinced ct nid lindex lhash
  debug $ "Sent AppendEntriesResponse: " ++ show ct

-- no state update but uses state
sendAllAppendEntriesResponse :: Monad m => Raft m ()
sendAllAppendEntriesResponse = do
  ct <- use term
  nid <- view (cfg.nodeId)
  (_, lindex, lhash) <- lastLogInfo <$> use logEntries
  aer <- return $ createAppendEntriesResponse' True True ct nid lindex lhash
  oNodes <- view (cfg.otherNodes)
  sendRPCs $ (,aer) <$> Set.toList oNodes

createRequestVoteResponse :: MonadWriter [String] m => Term -> LogIndex -> NodeID -> NodeID -> Bool -> m RequestVoteResponse
createRequestVoteResponse term' logIndex' myNodeId' target vote = do
  tell ["Created RequestVoteResponse: " ++ show term']
  return $ RequestVoteResponse term' logIndex' myNodeId' vote target NewMsg

-- no state update
sendResults :: Monad m => [(NodeID, CommandResponse)] -> Raft m ()
sendResults results = sendRPCs $ second CMDR' <$> results


-- TODO: figure out if there is a needless performance hit here (looking up these constants every time?)
sendRPC :: Monad m => NodeID -> RPC -> Raft m ()
sendRPC target rpc = do
  send <- view (rs.sendMessage)
  myNodeId <- view (cfg.nodeId)
  privKey <- view (cfg.myPrivateKey)
  pubKey <- view (cfg.myPublicKey)
  send target $ encode $ rpcToSignedRPC myNodeId pubKey privKey rpc

encodedRPC :: NodeID -> PrivateKey -> PublicKey -> RPC -> ByteString
encodedRPC myNodeId privKey pubKey rpc = encode $! rpcToSignedRPC myNodeId pubKey privKey rpc
{-# INLINE encodedRPC #-}

sendRPCs :: Monad m => [(NodeID, RPC)] -> Raft m ()
sendRPCs rpcs = do
  send <- view (rs.sendMessages)
  myNodeId <- view (cfg.nodeId)
  privKey <- view (cfg.myPrivateKey)
  pubKey <- view (cfg.myPublicKey)
  msgs <- return ((second (encodedRPC myNodeId privKey pubKey) <$> rpcs ) `using` parList rseq)
  send msgs
