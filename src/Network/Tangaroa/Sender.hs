module Network.Tangaroa.Sender
  ( sendAppendEntries
  , sendAppendEntriesResponse
  , sendRequestVote
  , sendRequestVoteResponse
  , sendAllAppendEntries
  , sendAllRequestVotes
  , sendResults
  , sendRPC
  ) where

import Control.Lens hiding (Index)
import Data.Foldable (traverse_)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Network.Tangaroa.Util
import Network.Tangaroa.Types

sendAppendEntries :: Ord nt => nt -> Raft nt et rt mt ()
sendAppendEntries target = do
  mni <- use $ lNextIndex.at target
  es <- use logEntries
  let (pli,plt) = logInfoForNextIndex mni es
  ct <- use term
  nid <- view (cfg.nodeId)
  ci <- use commitIndex
  debug $ "sendAppendEntries: " ++ show ct
  sendRPC target $ AE $
    AppendEntries ct nid pli plt (Seq.drop (pli + 1) es) ci

sendAppendEntriesResponse :: nt -> Bool -> Index -> Raft nt et rt mt ()
sendAppendEntriesResponse target success lindex = do
  ct <- use term
  nid <- view (cfg.nodeId)
  debug $ "sendAppendEntriesResponse: " ++ show ct
  sendRPC target $ AER $ AppendEntriesResponse ct nid success lindex

sendRequestVote :: nt -> Raft nt et rt mt ()
sendRequestVote target = do
  ct <- use term
  nid <- view (cfg.nodeId)
  es <- use logEntries
  let (llt, lli) = lastLogInfo es
  debug $ "sendRequestVote: " ++ show ct
  sendRPC target $ RV $ RequestVote ct nid lli llt

sendRequestVoteResponse :: nt -> Bool -> Raft nt et rt mt ()
sendRequestVoteResponse target vote = do
  ct <- use term
  nid <- view (cfg.nodeId)
  debug $ "sendRequestVoteResponse: " ++ show ct
  sendRPC target $ RVR $ RequestVoteResponse ct nid vote

sendAllAppendEntries :: Ord nt => Raft nt et rt mt ()
sendAllAppendEntries = traverse_ sendAppendEntries =<< view (cfg.otherNodes)

sendAllRequestVotes :: Raft nt et rt mt ()
sendAllRequestVotes = traverse_ sendRequestVote =<< use cPotentialVotes

sendResults :: Seq (rt,nt) -> Raft nt et rt mt ()
sendResults results =
  traverse_ (\(result,target) -> sendRPC target $ CMDR result) results

-- TODO: check this
-- called by leaders sending appendEntries.
-- given a replica's nextIndex, get the index and term to send as
-- prevLog(Index/Term)
logInfoForNextIndex :: Maybe Index -> Seq (Term,et) -> (Index,Term)
logInfoForNextIndex mni es =
  case mni of
    Just ni -> let pli = ni - 1 in
      case seqIndex es pli of
        Just (t,_) -> (pli, t)
         -- this shouldn't happen, because nextIndex - 1 should always be at
         -- most our last entry
        Nothing -> (startIndex, startTerm)
    Nothing -> (startIndex, startTerm)

sendRPC :: nt -> RPC nt et rt -> Raft nt et rt mt ()
sendRPC target rpc = do
  send <- view (rs.sendMessage)
  ser <- view (rs.serializeRPC)
  send target $ ser rpc
