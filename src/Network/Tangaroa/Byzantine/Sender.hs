module Network.Tangaroa.Byzantine.Sender
  ( sendAppendEntries
  , sendAppendEntriesResponse
  , sendRequestVote
  , sendRequestVoteResponse
  , sendAllAppendEntries
  , sendAllRequestVotes
  , sendResults
  , sendRPC
  , sendSignedRPC
  ) where

import Control.Lens
import Data.Binary
import Data.Foldable (traverse_)
import Data.Functor
import Data.Sequence (Seq)
import Data.Set (Set)
import qualified Data.ByteString.Lazy as B
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Network.Tangaroa.Byzantine.Util
import Network.Tangaroa.Byzantine.Types

sendAppendEntries :: (Binary nt, Binary et, Binary rt, Ord nt) => nt -> Raft nt et rt mt ()
sendAppendEntries target = do
  mni <- use $ lNextIndex.at target
  es <- use logEntries
  let (pli,plt) = logInfoForNextIndex mni es
  ct <- use term
  nid <- view (cfg.nodeId)
  ci <- use commitIndex
  debug $ "sendAppendEntries: " ++ show ct
  qVoteList <- getVotesForNode target
  sendSignedRPC target $ AE $
    AppendEntries ct nid pli plt (Seq.drop (pli + 1) es) ci qVoteList B.empty

getVotesForNode :: Ord nt => nt -> Raft nt et rt mt (Set (RequestVoteResponse nt))
getVotesForNode target = do
  convinced <- Set.member target <$> use lConvinced
  if convinced
    then return Set.empty
    else use cYesVotes

sendAppendEntriesResponse :: (Binary nt, Binary et, Binary rt) => nt -> Bool -> Bool -> LogIndex -> Raft nt et rt mt ()
sendAppendEntriesResponse target success convinced lindex = do
  ct <- use term
  nid <- view (cfg.nodeId)
  debug $ "sendAppendEntriesResponse: " ++ show ct
  sendSignedRPC target $ AER $ AppendEntriesResponse ct nid success convinced lindex B.empty

sendRequestVote :: (Binary nt, Binary et, Binary rt) => nt -> Raft nt et rt mt ()
sendRequestVote target = do
  ct <- use term
  nid <- view (cfg.nodeId)
  es <- use logEntries
  let (llt, lli) = lastLogInfo es
  debug $ "sendRequestVote: " ++ show ct
  sendSignedRPC target $ RV $ RequestVote ct nid lli llt B.empty

sendRequestVoteResponse :: (Binary nt, Binary et, Binary rt) => nt -> Bool -> Raft nt et rt mt ()
sendRequestVoteResponse target vote = do
  ct <- use term
  nid <- view (cfg.nodeId)
  debug $ "sendRequestVoteResponse: " ++ show ct
  sendSignedRPC target $ RVR $ RequestVoteResponse ct nid vote target B.empty

sendAllAppendEntries :: (Binary nt, Binary et, Binary rt, Ord nt) => Raft nt et rt mt ()
sendAllAppendEntries = traverse_ sendAppendEntries =<< view (cfg.otherNodes)

sendAllRequestVotes :: (Binary nt, Binary et, Binary rt) => Raft nt et rt mt ()
sendAllRequestVotes = traverse_ sendRequestVote =<< use cPotentialVotes

sendResults :: (Binary nt, Binary et, Binary rt) => Seq (nt, CommandResponse nt rt) -> Raft nt et rt mt ()
sendResults results = do
  traverse_ (\(target,cmdr) -> sendSignedRPC target $ CMDR cmdr) results

-- TODO: check this
-- called by leaders sending appendEntries.
-- given a replica's nextIndex, get the index and term to send as
-- prevLog(Index/Term)
logInfoForNextIndex :: Maybe LogIndex -> Seq (Term,et) -> (LogIndex,Term)
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

sendSignedRPC :: (Binary nt, Binary et, Binary rt) => nt -> RPC nt et rt -> Raft nt et rt mt ()
sendSignedRPC target rpc = do
  pk <- view (cfg.privateKey)
  sendRPC target $ case rpc of
    AE ae          -> AE   $ signRPC pk ae
    AER aer        -> AER  $ signRPC pk aer
    RV rv          -> RV   $ signRPC pk rv
    RVR rvr        -> RVR  $ signRPC pk rvr
    CMD cmd        -> CMD  $ signRPC pk cmd
    CMDR cmdr      -> CMDR $ signRPC pk cmdr
    REVOLUTION rev -> REVOLUTION $ signRPC pk rev
    _         -> rpc
