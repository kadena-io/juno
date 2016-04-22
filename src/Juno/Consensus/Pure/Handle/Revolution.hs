{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Juno.Consensus.Pure.Handle.Revolution
    (handle)
where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Map (Map)
import qualified Data.Map as Map
import Juno.Consensus.Pure.Types
import Juno.Util.Util (debug, getRevSigOrInvariantError)
import qualified Juno.Types as JT

data RevolutionEnv = RevolutionEnv {
    _lazyVote         :: Maybe (Term, NodeID, LogIndex) -- Handler
  , _currentLeader    :: Maybe NodeID -- Client,Handler,Role
  , _replayMap        :: Map (NodeID, Signature) (Maybe CommandResult) -- Handler
}
makeLenses ''RevolutionEnv

data RevolutionOut =
  UnknownNode |
  RevolutionCalledOnNonLeader |
  IgnoreLeader
    { _deleteReplayMapEntry :: (NodeID, Signature) } |
  IgnoreLeaderAndClearLazyVote
    { _deleteReplayMapEntry :: (NodeID, Signature) }

handleRevolution :: (MonadReader RevolutionEnv m, MonadWriter [String] m) => Revolution -> m RevolutionOut
handleRevolution rev@Revolution{..} = do
  currentLeader' <- view currentLeader
  replayMap' <- view replayMap
  revSig <- return $ getRevSigOrInvariantError "handleRevolution" rev
  if Map.notMember (_revClientId, revSig) replayMap'
  then
    case currentLeader' of
      Just l | l == _revLeaderId -> do
        -- clear our lazy vote if it was for this leader
        lazyVote' <- view lazyVote
        case lazyVote' of
          Just (_, lvid, _) | lvid == _revLeaderId -> return $ IgnoreLeaderAndClearLazyVote (_revClientId, revSig)
          _ -> return $ IgnoreLeader (_revClientId, revSig)
      _ -> return RevolutionCalledOnNonLeader
  else return UnknownNode

handle :: Monad m => Revolution -> JT.Raft m ()
handle msg = do
  s <- get
  (out,l) <- runReaderT (runWriterT (handleRevolution msg)) $
               RevolutionEnv
                 (JT._lazyVote s)
                 (JT._currentLeader s)
                 (JT._replayMap s)
  mapM_ debug l
  case out of
    UnknownNode -> return ()
    RevolutionCalledOnNonLeader -> return ()
    IgnoreLeader{..} -> do
      JT.replayMap %= Map.insert _deleteReplayMapEntry Nothing
      JT.ignoreLeader .= True
    IgnoreLeaderAndClearLazyVote{..} -> do
      JT.replayMap %= Map.insert _deleteReplayMapEntry Nothing
      JT.lazyVote .= Nothing
      JT.ignoreLeader .= True
