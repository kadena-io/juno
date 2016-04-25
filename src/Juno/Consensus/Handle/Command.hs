{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Juno.Consensus.Handle.Command
    (handle
    ,handleBatch)
    where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString as B
import qualified Data.Map as Map

import Juno.Consensus.Commit (makeCommandResponse')
import Juno.Types.Log
import Juno.Consensus.Handle.Types
import Juno.Runtime.Sender (sendRPC, createAppendEntriesResponse)
import Juno.Util.Util (getCmdSigOrInvariantError)
import qualified Juno.Types as JT

import Juno.Consensus.Handle.AppendEntriesResponse (updateCommitProofMap)

data CommandEnv = CommandEnv {
      _nodeRole :: Role
    , _term :: Term
    , _currentLeader :: Maybe NodeID
    , _replayMap :: Map.Map (NodeID, Signature) (Maybe CommandResult)
    , _nodeId :: NodeID
}
makeLenses ''CommandEnv

data CommandOut =
    UnknownLeader |
    RetransmitToLeader {
      _leaderId :: NodeID
    , _cmd :: RPC } |
    CommitAndPropagate {
      _newEntry :: LogEntry
    , _replayKey :: (NodeID, Signature)
    } |
    AlreadySeen |
    SendCommandResponse {
      _clientId :: NodeID
    , _signedReponse :: RPC
    }

-- THREAD: SERVER MAIN. updates state
handleCommand :: (MonadReader CommandEnv m,MonadWriter [String] m) => Command -> m CommandOut
handleCommand cmd@Command{..} = do
  tell ["got a command RPC"]
  r <- view nodeRole
  ct <- view term
  mlid <- view currentLeader
  replays <- view replayMap
  nid <- view nodeId
  cmdSig <- return $ getCmdSigOrInvariantError "handleCommand" cmd
  case (Map.lookup (_cmdClientId, cmdSig) replays, r, mlid) of
    (Just (Just result), _, _) -> do
      cmdr <- return $ makeCommandResponse' nid mlid cmd result
      return . SendCommandResponse _cmdClientId . CMDR' $ cmdr
      -- we have already committed this request, so send the result to the client
    (Just Nothing, _, _) ->
      -- we have already seen this request, but have not yet committed it
      -- nothing to do
      return AlreadySeen
    (_, Leader, _) ->
      -- we're the leader, so append this to our log with the current term
      -- and propagate it to replicas
      return $ CommitAndPropagate (LogEntry ct cmd B.empty) (_cmdClientId, cmdSig)
    (_, _, Just lid) ->
      -- we're not the leader, but we know who the leader is, so forward this
      -- command (don't sign it ourselves, as it comes from the client)
      return $ RetransmitToLeader lid (CMD' cmd)
    (_, _, Nothing) ->
      -- we're not the leader, and we don't know who the leader is, so can't do
      -- anything
      return UnknownLeader

handleSingleCommand :: Monad m => Command -> JT.Raft m ()
handleSingleCommand cmd = do
  c <- view JT.cfg
  s <- get
  (out,_) <- runReaderT (runWriterT (handleCommand cmd)) $
             CommandEnv (JT._nodeRole s)
                        (JT._term s)
                        (JT._currentLeader s)
                        (JT._replayMap s)
                        (JT._nodeId c)
  case out of
    UnknownLeader -> return ()
    AlreadySeen -> return ()
    (RetransmitToLeader lid rpc) -> sendRPC lid rpc
    (SendCommandResponse cid rpc) -> sendRPC cid rpc
    (CommitAndPropagate newEntry replayKey) -> do
               JT.logEntries %= appendLogEntry newEntry
               JT.replayMap %= Map.insert replayKey Nothing
               myEvidence <- createAppendEntriesResponse True True
               JT.commitProof %= updateCommitProofMap myEvidence

handle :: Monad m => Command -> JT.Raft m ()
handle cmd = handleSingleCommand cmd

handleBatch :: Monad m => CommandBatch -> JT.Raft m ()
handleBatch CommandBatch{..} = mapM_ handleSingleCommand _cmdbBatch
