{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Juno.Consensus.Handle.AppendEntriesResponse
  (handle
  ,handleAlotOfAers
  ,updateCommitProofMap)
where

import Control.Lens hiding (Index)
import Control.Parallel.Strategies
import Control.Monad.Reader
import Control.Monad.State (get)
import Control.Monad.Writer.Strict
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Juno.Consensus.Commit (doCommit)
import Juno.Consensus.Handle.Types

import Juno.Runtime.Timer (resetElectionTimerLeader)
import Juno.Util.Util (debug, updateLNextIndex)
import qualified Juno.Types as JT

data AEResponseEnv = AEResponseEnv {
-- Old Constructors
    _nodeRole             :: Role
  , _term             :: Term
  , _commitProof      :: Map NodeID AppendEntriesResponse
  }
makeLenses ''AEResponseEnv

data AEResponseOut = AEResponseOut
  { _stateMergeCommitProof :: Map NodeID AppendEntriesResponse
  , _leaderState :: LeaderState }

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
    , _setLaggingLogIndex :: LogIndex } |
  ConvincedAndSuccessful -- does not send AE after
    { _incrementNextIndexNode :: NodeID
    , _incrementNextIndexLogIndex :: LogIndex
    , _insertConvinced :: NodeID}


data Convinced = Convinced | NotConvinced
data AESuccess = Success | Failure
data RequestTermStatus = OldRequestTerm | CurrentRequestTerm | NewerRequestTerm

handleAEResponse :: (MonadWriter [String] m, MonadReader AEResponseEnv m) => AppendEntriesResponse -> m AEResponseOut
handleAEResponse aer@AppendEntriesResponse{..} = do
    --tell ["got an appendEntriesResponse RPC"]
    mcp <- updateCommitProofMap aer <$> view commitProof
    role' <- view nodeRole
    currentTerm' <- view term
    if (role' == Leader)
    then
      return $ case (isConvinced, isSuccessful, whereIsTheRequest currentTerm') of
        (NotConvinced, _, OldRequestTerm) -> AEResponseOut mcp $ Unconvinced _aerNodeId _aerNodeId
        (NotConvinced, _, CurrentRequestTerm) -> AEResponseOut mcp $ Unconvinced _aerNodeId _aerNodeId
        (Convinced, Failure, CurrentRequestTerm) -> AEResponseOut mcp $ ConvincedAndUnsuccessful _aerNodeId _aerIndex
        (Convinced, Success, CurrentRequestTerm) -> AEResponseOut mcp $ ConvincedAndSuccessful _aerNodeId _aerIndex _aerNodeId
        -- The next two case are underspecified currently and they should not occur as
        -- they imply that a follow is ahead of us but the current code sends an AER anyway
        (NotConvinced, _, _) -> AEResponseOut mcp $ StatelessSendAE _aerNodeId
        (_, Failure, _) -> AEResponseOut mcp $ StatelessSendAE _aerNodeId
        -- This is just a fall through case in the current code
        -- do nothing as the follower is convinced and successful but out of date? Shouldn't this trigger a replay AE?
        (Convinced, Success, OldRequestTerm) -> AEResponseOut mcp DoNothing
        -- We are a leader, in a term that hasn't happened yet?
        (_, _, NewerRequestTerm) -> AEResponseOut mcp DoNothing
    else return $ AEResponseOut mcp NotLeader
  where
    isConvinced = if _aerConvinced then Convinced else NotConvinced
    isSuccessful = if _aerSuccess then Success else Failure
    whereIsTheRequest ct | _aerTerm == ct = CurrentRequestTerm
                         | _aerTerm < ct = OldRequestTerm
                         | otherwise = NewerRequestTerm

updateCommitProofMap :: AppendEntriesResponse -> Map NodeID AppendEntriesResponse -> Map NodeID AppendEntriesResponse
updateCommitProofMap aerNew m = Map.alter go nid m
  where
    nid :: NodeID
    nid = _aerNodeId aerNew
    go = \case
      Nothing   -> Just aerNew
      Just aerOld -> if _aerIndex aerNew > _aerIndex aerOld
                     then -- NB: we don't check the hash here for a couple reasons.
                          --   - The LogEntry for the index may not exist, but the AER may be valid in the
                          --     future when new entries are added
                          --   - The node may be giving bad hashes always, in which case we really don't care
                          --     as every node can only give evidence once per doCommit cycle
                          --   - It's more efficient to check later as we may get many AER's from that node
                          --     in a given batch cycle
                       Just aerNew
                     else Just aerOld

handle :: Monad m => AppendEntriesResponse -> JT.Raft m ()
handle ae = do
  s <- get
  let ape = AEResponseEnv
              (JT._nodeRole s)
              (JT._term s)
              (JT._commitProof s)
  (AEResponseOut{..}, l) <- runReaderT (runWriterT (handleAEResponse ae)) ape
  mapM_ debug l
  JT.commitProof .= _stateMergeCommitProof
  doCommit
  case _leaderState of
    NotLeader -> return ()
    DoNothing -> resetElectionTimerLeader
    StatelessSendAE{..} ->
      resetElectionTimerLeader
    Unconvinced{..} -> do
      JT.lConvinced %= Set.delete _deleteConvinced
      resetElectionTimerLeader
    ConvincedAndSuccessful{..} -> do
      updateLNextIndex $ Map.insert _incrementNextIndexNode $ _incrementNextIndexLogIndex + 1
      JT.lConvinced %= Set.insert _insertConvinced
      resetElectionTimerLeader
    ConvincedAndUnsuccessful{..} -> do
      updateLNextIndex $ Map.insert _sendAENodeID _setLaggingLogIndex
      resetElectionTimerLeader

handleAlotOfAers :: Monad m => AlotOfAERs -> JT.Raft m ()
handleAlotOfAers (AlotOfAERs m) = do
  ks <- KeySet <$> view (JT.cfg . JT.publicKeys) <*> view (JT.cfg . JT.clientPublicKeys)
  res <- return ((processSetAer ks <$> Map.elems m) `using` parList rseq)
  aers <- catMaybes <$> mapM (\(a,l) -> mapM_ debug l >> return a) res
  mapM_ handle aers

processSetAer :: KeySet -> Set AppendEntriesResponse -> (Maybe AppendEntriesResponse, [String])
processSetAer ks s = go [] (Set.toDescList s)
  where
    go fails [] = (Nothing, fails)
    go fails (aer:rest)
      | _aerWasVerified aer = (Just aer, fails)
      | otherwise = case aerReVerify ks aer of
                      Left f -> go (f:fails) rest
                      Right () -> (Just $ aer {_aerWasVerified = True}, fails)


-- | Verify if needed on `ReceivedMsg` provenance
aerReVerify :: KeySet -> AppendEntriesResponse -> Either String ()
aerReVerify  _ AppendEntriesResponse{ _aerWasVerified = True } = Right ()
aerReVerify  _ AppendEntriesResponse{ _aerProvenance = NewMsg } = Right ()
aerReVerify ks AppendEntriesResponse{
                  _aerWasVerified = False,
                  _aerProvenance = ReceivedMsg{..}
                } = verifySignedRPC ks $ SignedRPC _pDig _pOrig
