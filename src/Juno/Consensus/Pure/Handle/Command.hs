{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Juno.Consensus.Pure.Handle.Command
    (handle)
    where

import Codec.Crypto.RSA
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString as B
import Data.ByteString.Lazy as LB
import qualified Data.Map as Map

import Juno.Consensus.ByzRaft.Commit (doCommit,makeCommandResponse')
import Juno.Consensus.ByzRaft.Log (addLogEntryAndHash)
import Juno.Consensus.Pure.Types
import Juno.Runtime.Sender (sendRPC,sendAllAppendEntries,sendAllAppendEntriesResponse)
import Juno.Util.Util (debug)

import qualified Juno.Runtime.Types as JT


data CommandEnv = CommandEnv {
      _role :: Role
    , _term :: Term
    , _currentLeader :: Maybe NodeID
    , _replayMap :: Map.Map (NodeID, LB.ByteString) (Maybe CommandResult)
    , _nodeId :: NodeID
    , _privateKey :: PrivateKey
}
makeLenses ''CommandEnv

data CommandOut =
    UnknownLeader |
    RetransmitToLeader {
      _leaderId :: NodeID
    , _cmd :: RPC } |
    CommitAndPropagate {
      _newEntry :: LogEntry
    , _replayKey :: (NodeID, LB.ByteString)
    } |
    AlreadySeen |
    SendCommandResponse {
      _clientId :: NodeID
    , _signedReponse :: RPC
    }


{-
original handler function:

-- THREAD: SERVER MAIN. updates state
handleCommand :: Monad m => Command -> Raft m ()
handleCommand cmd@Command{..} = do
  debug $ "got a command RPC"
  r <- use role
  ct <- use term
  mlid <- use currentLeader
  replays <- use replayMap
  case (Map.lookup (_cmdClientId, _cmdSig) replays, r, mlid) of
    (Just (Just result), _, _) -> do
      cmdr <- makeCommandResponse cmd result
      sendSignedRPC _cmdClientId $ CMDR cmdr
      -- we have already committed this request, so send the result to the client
    (Just Nothing, _, _) ->
      -- we have already seen this request, but have not yet committed it
      -- nothing to do
      return ()
    (_, Leader, _) -> do
      -- we're the leader, so append this to our log with the current term
      -- and propagate it to replicas
      logEntries %= addLogEntryAndHash (LogEntry ct cmd B.empty)
      replayMap %= Map.insert (_cmdClientId, _cmdSig) Nothing
      sendAllAppendEntries
      sendAllAppendEntriesResponse
      doCommit
    (_, _, Just lid) ->
      -- we're not the leader, but we know who the leader is, so forward this
      -- command (don't sign it ourselves, as it comes from the client)
      sendRPC lid $ CMD cmd -- THREAD: one-off, no state
    (_, _, Nothing) ->
      -- we're not the leader, and we don't know who the leader is, so can't do
      -- anything
      return ()

-}

-- THREAD: SERVER MAIN. updates state
handleCommand :: (MonadReader CommandEnv m,MonadWriter [String] m) => Command -> m CommandOut
handleCommand cmd@Command{..} = do
  tell ["got a command RPC"]
  r <- view role
  ct <- view term
  mlid <- view currentLeader
  replays <- view replayMap
  nid <- view nodeId
  case (Map.lookup (_cmdClientId, _cmdSig) replays, r, mlid) of
    (Just (Just result), _, _) -> do
      cmdr <- return $ makeCommandResponse' nid mlid cmd result
      pk <- view privateKey
      return . SendCommandResponse _cmdClientId . CMDR $ signRPC pk cmdr
      -- we have already committed this request, so send the result to the client
    (Just Nothing, _, _) ->
      -- we have already seen this request, but have not yet committed it
      -- nothing to do
      return AlreadySeen
    (_, Leader, _) -> do
      -- we're the leader, so append this to our log with the current term
      -- and propagate it to replicas
      return $ CommitAndPropagate (LogEntry ct cmd B.empty) (_cmdClientId, _cmdSig)
    (_, _, Just lid) ->
      -- we're not the leader, but we know who the leader is, so forward this
      -- command (don't sign it ourselves, as it comes from the client)
      return $ RetransmitToLeader lid (CMD cmd)
    (_, _, Nothing) ->
      -- we're not the leader, and we don't know who the leader is, so can't do
      -- anything
      return UnknownLeader

handle :: Monad m => Command -> JT.Raft m ()
handle cmd = do
  c <- view JT.cfg
  s <- get
  (out,l) <- runReaderT (runWriterT (handleCommand cmd)) $
             CommandEnv (JT._role s)
                        (JT._term s)
                        (JT._currentLeader s)
                        (JT._replayMap s)
                        (JT._nodeId c)
                        (JT._privateKey c)

  mapM_ debug l
  case out of
    UnknownLeader -> return ()
    AlreadySeen -> return ()
    (RetransmitToLeader lid rpc) -> sendRPC lid rpc
    (SendCommandResponse cid rpc) -> sendRPC cid rpc
    (CommitAndPropagate newEntry replayKey) -> do
               JT.logEntries %= addLogEntryAndHash newEntry
               JT.replayMap %= Map.insert replayKey Nothing
               sendAllAppendEntries
               sendAllAppendEntriesResponse
               doCommit
