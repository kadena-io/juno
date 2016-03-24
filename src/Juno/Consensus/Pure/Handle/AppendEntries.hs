{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Juno.Consensus.Pure.Handle.AppendEntries
  (handle)
where

import Control.Lens hiding (Index)
import Control.Monad.Reader
import Control.Monad.State (get)
import Control.Monad.Writer.Strict
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import Juno.Consensus.ByzRaft.Log
import Juno.Consensus.Pure.Types
import Juno.Runtime.Sender (sendAllAppendEntriesResponse, sendAppendEntriesResponse)
import Juno.Runtime.Timer (resetElectionTimer)
import Juno.Util.Util (seqIndex, debug, setTerm, setRole, setCurrentLeader,
                       getCmdSigOrInvariantError)
import qualified Juno.Runtime.Types as JT

data AppendEntriesEnv = AppendEntriesEnv {
-- Old Constructors
    _term             :: Term
  , _currentLeader    :: Maybe NodeID
  , _ignoreLeader     :: Bool
  , _logEntries       :: Seq LogEntry
-- New Constructors
  , _quorumSize       :: Int
  }
makeLenses ''AppendEntriesEnv

data AppendEntriesOut = AppendEntriesOut {
      _newLeaderAction :: CheckForNewLeaderOut
    , _result :: AppendEntriesResult
}

data CheckForNewLeaderOut =
  LeaderUnchanged |
  NewLeaderConfirmed {
      _stateRsUpdateTerm  :: Term
    , _stateIgnoreLeader  :: Bool
    , _stateCurrentLeader :: NodeID
    , _stateRole          :: Role
    }

data AppendEntriesResult =
    Ignore |
    SendUnconvincedResponse {
      _responseLeaderId :: NodeID } |
    ValidLeaderAndTerm {
        _responseLeaderId :: NodeID
      , _validReponse :: ValidResponse }

data ValidResponse =
    SendFailureResponse |
    Commit {
        _replay :: Map (NodeID, Signature) (Maybe CommandResult)
      , _updatedLog :: Seq LogEntry }

-- THREAD: SERVER MAIN. updates state
handleAppendEntries :: (MonadWriter [String] m, MonadReader AppendEntriesEnv m) => AppendEntries -> m AppendEntriesOut
handleAppendEntries ae@AppendEntries{..} = do
  tell ["got an appendEntries RPC: prev log entry: Index " ++ show _prevLogIndex ++ " " ++ show _prevLogTerm]
  nlo <- checkForNewLeader ae
  (currentLeader',ignoreLeader',currentTerm' ) :: (Maybe NodeID,Bool,Term) <-
                case nlo of
                  LeaderUnchanged -> (,,) <$> view currentLeader <*> view ignoreLeader <*> view term
                  NewLeaderConfirmed{..} -> return (Just _stateCurrentLeader,_stateIgnoreLeader,_stateRsUpdateTerm)
  case currentLeader' of
    Just leader' | not ignoreLeader' && leader' == _leaderId && _aeTerm == currentTerm' -> do
      plmatch <- prevLogEntryMatches _prevLogIndex _prevLogTerm
      if not plmatch
        then return $ AppendEntriesOut nlo $ ValidLeaderAndTerm _leaderId $ SendFailureResponse
        else AppendEntriesOut nlo . ValidLeaderAndTerm _leaderId <$> appendLogEntries _prevLogIndex _aeEntries
          {-|
          if (not (Seq.null _aeEntries))
            -- only broadcast when there are new entries
            -- this has the downside that recovering nodes won't update
            -- their commit index until new entries come along
            -- not sure if this is okay or not
            -- committed entries by definition have already been externalized
            -- so if a particular node missed it, there were already 2f+1 nodes
            -- that didn't
            then sendAllAppendEntriesResponse
            else sendAppendEntriesResponse _leaderId True True
          --}
    _ | not ignoreLeader' && _aeTerm >= currentTerm' -> do -- see TODO about setTerm
      tell ["sending unconvinced response"]
      return $ AppendEntriesOut nlo $ SendUnconvincedResponse _leaderId
    _ -> return $ AppendEntriesOut nlo Ignore

checkForNewLeader :: (MonadWriter [String] m, MonadReader AppendEntriesEnv m) => AppendEntries -> m CheckForNewLeaderOut
checkForNewLeader AppendEntries{..} = do
  term' <- view term
  currentLeader' <- view currentLeader
  if (_aeTerm == term' && currentLeader' == Just _leaderId) || _aeTerm < term' || Set.size _aeQuorumVotes == 0
  then return LeaderUnchanged
  else do
     tell ["New leader identified: " ++ show _leaderId]
     votesValid <- confirmElection _leaderId _aeTerm _aeQuorumVotes
     tell ["New leader votes are valid: " ++ show votesValid]
     if votesValid
     then return $ NewLeaderConfirmed
          _aeTerm
          False
          _leaderId
          Follower
     else return LeaderUnchanged

confirmElection :: (MonadWriter [String] m, MonadReader AppendEntriesEnv m) => NodeID -> Term -> Set RequestVoteResponse -> m Bool
confirmElection leader' term' votes = do
  quorumSize' <- view quorumSize
  tell ["confirming election of a new leader"]
  if Set.size votes >= quorumSize'
    then return $ all (validateVote leader' term') votes
    else return False

validateVote :: NodeID -> Term -> RequestVoteResponse -> Bool
validateVote leader' term' RequestVoteResponse{..} = (_rvrCandidateId == leader' && _rvrTerm == term')


prevLogEntryMatches :: MonadReader AppendEntriesEnv m => LogIndex -> Term -> m Bool
prevLogEntryMatches pli plt = do
  es <- view logEntries
  case seqIndex es $ fromIntegral pli of
    -- if we don't have the entry, only return true if pli is startIndex
    Nothing    -> return (pli == startIndex)
    -- if we do have the entry, return true if the terms match
    Just LogEntry{..} -> return (_leTerm == plt)

appendLogEntries :: (MonadWriter [String] m, MonadReader AppendEntriesEnv m)
                 => LogIndex -> Seq LogEntry -> m ValidResponse
appendLogEntries pli newEs = do
  les <- view logEntries
  logEntries' <- return . (Seq.>< newEs) . Seq.take (fromIntegral pli + 1) $ les
  replay <- return $
    foldl (\m LogEntry{_leCommand = c@Command{..}} ->
            Map.insert (_cmdClientId, getCmdSigOrInvariantError "appendLogEntries" c) Nothing m)
    Map.empty newEs
  logEntries'' <- return $ updateLogHashesFromIndex (pli + 1) logEntries'
  tell ["replaying LogEntry(s): " ++ show (Seq.length les) ++ " through " ++ show (Seq.length logEntries'') ]
  return $ Commit replay logEntries''

applyNewLeader :: Monad m => CheckForNewLeaderOut -> JT.Raft m ()
applyNewLeader LeaderUnchanged = return ()
applyNewLeader NewLeaderConfirmed{..} = do
  setTerm _stateRsUpdateTerm
  JT.ignoreLeader .= _stateIgnoreLeader
  setCurrentLeader $ Just _stateCurrentLeader
  setRole _stateRole

handle :: Monad m => AppendEntries -> JT.Raft m ()
handle ae = do
  r <- ask
  s <- get
  let ape = AppendEntriesEnv
              (JT._term s)
              (JT._currentLeader s)
              (JT._ignoreLeader s)
              (JT._logEntries s)
              (JT._quorumSize r)
  (AppendEntriesOut{..}, l) <- runReaderT (runWriterT (handleAppendEntries ae)) ape
  mapM_ debug l
  applyNewLeader _newLeaderAction
  case _result of
    Ignore -> return ()
    SendUnconvincedResponse{..} -> sendAppendEntriesResponse _responseLeaderId False False
    ValidLeaderAndTerm{..} -> do
      resetElectionTimer
      JT.lazyVote .= Nothing
      case _validReponse of
        SendFailureResponse -> sendAppendEntriesResponse _responseLeaderId False True
        (Commit rMap updatedLog') -> do
          JT.logEntries .= updatedLog'
          JT.replayMap %= Map.union rMap
          sendAllAppendEntriesResponse
