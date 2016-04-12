{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Juno.Consensus.Pure.Handle.HeartbeatTimeout
    (handle)
where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Juno.Consensus.Pure.Types
import Juno.Runtime.Sender (sendAllAppendEntries)
import Juno.Runtime.Timer (resetHeartbeatTimer, hasElectionTimerLeaderFired)
import Juno.Util.Util (debug,enqueueEvent)
import qualified Juno.Runtime.Types as JT

data HeartbeatTimeoutEnv = HeartbeatTimeoutEnv {
      _role :: Role
    , _leaderWithoutFollowers :: Bool
}
makeLenses ''HeartbeatTimeoutEnv

data HeartbeatTimeoutOut = IsLeader | NotLeader | NoFollowers

handleHeartbeatTimeout :: (MonadReader HeartbeatTimeoutEnv m, MonadWriter [String] m) => String -> m HeartbeatTimeoutOut
handleHeartbeatTimeout s = do
  tell ["heartbeat timeout: " ++ s]
  role' <- view role
  leaderWithoutFollowers' <- view leaderWithoutFollowers
  case role' of
    Leader -> if leaderWithoutFollowers'
              then tell ["Leader found to not have followers"] >> return NoFollowers
              else return $ IsLeader
    _ -> return $ NotLeader

handle :: Monad m => String -> JT.Raft m ()
handle msg = do
  s <- get
  leaderWithoutFollowers' <- hasElectionTimerLeaderFired
  (out,l) <- runReaderT (runWriterT (handleHeartbeatTimeout msg)) $
             HeartbeatTimeoutEnv
             (JT._role s)
             leaderWithoutFollowers'
  mapM_ debug l
  case out of
    IsLeader -> do
      sendAllAppendEntries
      resetHeartbeatTimer
      hbMicrosecs <- view (JT.cfg . JT.heartbeatTimeout)
      JT.timeSinceLastAER %= (+ hbMicrosecs)
    NotLeader -> JT.timeSinceLastAER .= 0 -- probably overkill, but nice to know this gets set to 0 if not leader
    NoFollowers -> do
      timeout' <- return $ (JT._timeSinceLastAER s)
      enqueueEvent $ (ElectionTimeout $ "Leader has not hear from followers in: " ++ show (timeout' `div` 1000) ++ "ms")
