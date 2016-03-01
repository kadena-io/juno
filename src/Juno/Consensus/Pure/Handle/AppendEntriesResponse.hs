{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Juno.Consensus.Pure.Handle.AppendEntriesResponse
  (handle)
where

import Control.Lens hiding (Index)
import Control.Monad.Reader
import Control.Monad.State (get)
import Control.Monad.Writer.Strict
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Juno.Consensus.ByzRaft.Commit (doCommit)
import Juno.Consensus.Pure.Types
import Juno.Runtime.Sender (sendAppendEntries)
import Juno.Runtime.Timer (resetElectionTimerLeader)
import Juno.Util.Util (debug)
import qualified Juno.Runtime.Types as JT

data AEResponseEnv = AEResponseEnv {
-- Old Constructors
    _role             :: Role
  , _term             :: Term
  , _commitIndex      :: LogIndex
  , _commitProof      :: Map LogIndex (Set AppendEntriesResponse)
  }
makeLenses ''AEResponseEnv

data AEResponseOut = AEResponseOut
  { _stateMergeCommitProof :: MergeCommitProof
  , _leaderState :: LeaderState }

data MergeCommitProof =
  Ignore |
  SetFirstCommitProof {
    _stateCommitProof :: (LogIndex, (Set AppendEntriesResponse))} |
  AddToCommitProof {
    _stateAdditionCommitProof :: (LogIndex, AppendEntriesResponse) }

data LeaderState =
  DoNothing |
  NotLeader |
  StatelessSendAE
    { _sendAENodeID :: NodeID } |
  Unconvinced -- sends AE after
    { _sendAENodeID :: NodeID
    , _deleteConvinced :: NodeID } |
  ConvincedAndUnsuccessful -- sends AE after
    { _sendAENodeID :: NodeID
    , _decrementNextIndex :: NodeID } |
  ConvincedAndSuccessful -- does not send AE after
    { _incrementNextIndexNode :: NodeID
    , _incrementNextIndexLogIndex :: LogIndex
    , _insertConvinced :: NodeID}


data Convinced = Convinced | NotConvinced
data AESuccess = Success | Failure
data RequestTermStatus = OldRequestTerm | CurrentRequestTerm | NewerRequestTerm

handleAEResponse :: (MonadWriter [String] m, MonadReader AEResponseEnv m) => AppendEntriesResponse -> m AEResponseOut
handleAEResponse aer@AppendEntriesResponse{..} = do
    tell ["got an appendEntriesResponse RPC"]
    mcp <- mergeCommitProof aer
    role' <- view role
    currentTerm' <- view term
    if (role' == Leader)
    then
      return $ case (isConvinced, isSuccessful, whereIsTheRequest currentTerm') of
        (NotConvinced, _, OldRequestTerm) -> AEResponseOut mcp $ Unconvinced _aerNodeId _aerNodeId
        (NotConvinced, _, CurrentRequestTerm) -> AEResponseOut mcp $ Unconvinced _aerNodeId _aerNodeId
        (Convinced, Failure, CurrentRequestTerm) -> AEResponseOut mcp $ ConvincedAndUnsuccessful _aerNodeId _aerNodeId
        (Convinced, Success, CurrentRequestTerm) -> AEResponseOut mcp $ ConvincedAndSuccessful _aerNodeId _aerIndex _aerNodeId
        -- The next two case are underspecified currently and they should not occur as
        -- they imply that a follow is ahead of us but the current code sends an AER anyway
        (NotConvinced, _, _) -> AEResponseOut mcp $ StatelessSendAE _aerNodeId
        (_, Failure, _) -> AEResponseOut mcp $ StatelessSendAE _aerNodeId
        -- This is just a fall through case in the current code
        -- do nothing as the follower is convinced and successful but out of date? Shouldn't this trigger a replay AE?
        (Convinced, Success, OldRequestTerm) -> AEResponseOut mcp DoNothing
        -- We are a leader, in a term that hasn't happened yet?
        (_, _, NewerRequestTerm) -> AEResponseOut mcp $ DoNothing
    else return $ AEResponseOut mcp NotLeader
  where
    isConvinced = if _aerConvinced then Convinced else NotConvinced
    isSuccessful = if _aerSuccess then Success else Failure
    whereIsTheRequest ct = if _aerTerm == ct
                        then CurrentRequestTerm
                        else if _aerTerm < ct then OldRequestTerm else NewerRequestTerm

mergeCommitProof :: (MonadWriter [String] m, MonadReader AEResponseEnv m) => AppendEntriesResponse -> m MergeCommitProof
mergeCommitProof aer@AppendEntriesResponse{..} = do
  commitIndex' <- view commitIndex
  tell [ "merging commit proof for index: " ++ show _aerIndex]
  if (_aerIndex > commitIndex')
    then do
      commitProof' <- view (commitProof . at _aerIndex)
      case commitProof' of
        Nothing -> return $ SetFirstCommitProof (_aerIndex, Set.singleton aer)
        Just _ -> return $ AddToCommitProof (_aerIndex, aer)
    else return Ignore

applyMergeCommitProof :: Monad m => MergeCommitProof -> JT.Raft m ()
applyMergeCommitProof Ignore = return ()
applyMergeCommitProof (SetFirstCommitProof (aerIndex', proofSet')) =
  JT.commitProof %= Map.insert aerIndex' proofSet'
applyMergeCommitProof (AddToCommitProof (aerIndex', proofToInsert')) =
  JT.commitProof %= Map.adjust (Set.insert proofToInsert') aerIndex'

handle :: Monad m => AppendEntriesResponse -> JT.Raft m ()
handle ae = do
  s <- get
  let ape = AEResponseEnv
              (JT._role s)
              (JT._term s)
              (JT._commitIndex s)
              (JT._commitProof s)
  (AEResponseOut{..}, l) <- runReaderT (runWriterT (handleAEResponse ae)) ape
  mapM_ debug l
  applyMergeCommitProof _stateMergeCommitProof
  doCommit
  case _leaderState of
    NotLeader -> return ()
    DoNothing -> resetElectionTimerLeader
    StatelessSendAE{..} -> do
      sendAppendEntries _sendAENodeID
      resetElectionTimerLeader
    Unconvinced{..} -> do
      JT.lConvinced %= Set.delete _deleteConvinced
      sendAppendEntries _sendAENodeID
      resetElectionTimerLeader
    ConvincedAndSuccessful{..} -> do
      JT.lNextIndex . at _incrementNextIndexNode .= Just (_incrementNextIndexLogIndex + 1)
      JT.lConvinced %= Set.insert _insertConvinced
      resetElectionTimerLeader
    ConvincedAndUnsuccessful{..} -> do
      JT.lNextIndex %= Map.adjust (subtract 1) _decrementNextIndex
      sendAppendEntries _sendAENodeID
      resetElectionTimerLeader
