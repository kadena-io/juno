{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Juno.Runtime.Types
  ( Raft
  , RaftSpec(..)
  , readLogEntry, writeLogEntry, readTermNumber, writeTermNumber
  , readVotedFor, writeVotedFor, applyLogEntry
  , sendMessage, getMessage, debugPrint
  , random, enqueue, dequeue, enqueueLater, killEnqueued
  , NodeID(..)
  , CommandEntry(..)
  , CommandResult(..)
  , CommandStatus(..)
  , Term(..), startTerm
  , LogIndex(..), startIndex
  , RequestId(..), startRequestId, toRequestId
  , Config(..), otherNodes, nodeId, electionTimeoutRange, heartbeatTimeout
  , enableDebug, publicKeys, clientPublicKeys, myPrivateKey, clientTimeoutLimit
  , myPublicKey
  , Role(..)
  , RaftEnv(..), cfg, quorumSize, rs
  , LogEntry(..)
  , RaftState(..), role, term, votedFor, lazyVote, currentLeader, ignoreLeader
  , logEntries, commitIndex, commitProof, lastApplied, timerThread, replayMap
  , cYesVotes, cPotentialVotes, lNextIndex, lMatchIndex, lConvinced
  , numTimeouts, pendingRequests, currentRequestId, timeSinceLastAER
  , initialRaftState
  -- * RPC
  , AppendEntries(..)
  , AppendEntriesResponse(..)
  , RequestVote(..)
  , RequestVoteResponse(..)
  , Command(..)
  , CommandResponse(..)
  , Revolution(..)
  , RPC(..)
  , Event(..)
  , MsgType(..), KeySet(..), Digest(..), Provenance(..), WireFormat(..)
  , signedRPCtoRPC, rpcToSignedRPC
  , SignedRPC(..)
  -- for simplicity, re-export the crypto types
  , PublicKey(..), SecretKey(..), Signature(..), dsign, dverify, toPublicKey
  -- for testing & benchmarks
  , LEWire(..), encodeLEWire, decodeLEWire, decodeRVRWire
  , verifySignedRPC, CMDWire(..)
  ) where

import Control.Concurrent (ThreadId)
import Control.Lens hiding (Index, (|>))
import Control.Monad.RWS (RWST)
import Crypto.Sign.Ed25519 (PublicKey(..), SecretKey(..), Signature(..), dsign, dverify, toPublicKey)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.ByteString (ByteString)

import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Data.Word (Word64)
import Data.Foldable

import qualified Data.Binary.Serialise.CBOR.Class as CBC

import GHC.Int (Int64)
import GHC.Generics

import System.Random (Random)

newtype CommandEntry = CommandEntry { unCommandEntry :: ByteString }
  deriving (Show, Eq, Ord, Generic, Serialize, CBC.Serialise)

newtype CommandResult = CommandResult { unCommandResult :: ByteString }
  deriving (Show, Eq, Ord, Generic, Serialize)

data NodeID = NodeID { _host :: !String, _port :: !Word64 }
  deriving (Eq,Ord,Read,Show,Generic)
instance Serialize NodeID

newtype Term = Term Int
  deriving (Show, Read, Eq, Enum, Num, Ord, Generic, Serialize, CBC.Serialise)

startTerm :: Term
startTerm = Term (-1)

newtype LogIndex = LogIndex Int
  deriving (Show, Read, Eq, Ord, Enum, Num, Real, Integral, Generic, Serialize, CBC.Serialise)

startIndex :: LogIndex
startIndex = LogIndex (-1)

newtype RequestId = RequestId Int64
  deriving (Show, Read, Eq, Ord, Enum, Num, Generic, Serialize, CBC.Serialise)

startRequestId :: RequestId
startRequestId = RequestId 0

toRequestId :: Int64 -> RequestId
toRequestId a = RequestId a

data Config = Config
  { _otherNodes           :: !(Set NodeID)
  , _nodeId               :: !NodeID
  , _publicKeys           :: !(Map NodeID PublicKey)
  , _clientPublicKeys     :: !(Map NodeID PublicKey)
  , _myPrivateKey         :: !SecretKey
  , _myPublicKey          :: !PublicKey
  , _electionTimeoutRange :: !(Int,Int)
  , _heartbeatTimeout     :: !Int
  , _enableDebug          :: !Bool
  , _clientTimeoutLimit   :: !Int
  }
  deriving (Show, Generic)
makeLenses ''Config

data KeySet = KeySet
  { _ksCluster :: !(Map NodeID PublicKey)
  , _ksClient  :: !(Map NodeID PublicKey)
  } deriving (Show)

-- | One way or another we need a way to figure our what set of public keys to use for verification of signatures.
-- By placing the message type in the digest, we can make the WireFormat implementation easier as well. CMD and REV
-- need to use the Client Public Key maps.
data MsgType = AE | AER | RV | RVR | CMD | CMDR | REV
  deriving (Show, Eq, Ord, Generic)
instance Serialize MsgType

-- | Digest containing Sender ID, Signature, Sender's Public Key and the message type
data Digest = Digest
  { _digNodeId :: !NodeID
  , _digSig    :: !Signature
  , _digPubkey :: !PublicKey
  , _digType   :: !MsgType
  } deriving (Show, Eq, Ord, Generic)
instance Serialize Signature
instance Serialize PublicKey
deriving instance Read PublicKey
deriving instance Read SecretKey
instance Serialize Digest

-- | Type that is serialized and sent over the wire
data SignedRPC = SignedRPC
  { _sigDigest :: !Digest
  , _sigBody   :: !ByteString
  } deriving (Show, Eq, Generic)
instance Serialize SignedRPC

-- | Provenance is used to track if we made the message or received it. This is important for re-transmission.
data Provenance =
  NewMsg |
  ReceivedMsg
    { _pDig :: Digest
    , _pOrig :: ByteString }
  deriving (Show, Eq, Ord, Generic)
instance Serialize Provenance -- this instance is used for persistence, not sure we need it though

-- | Based on the MsgType in the SignedRPC's Digest, we know which set of keys are needed to validate the message
verifySignedRPC :: KeySet -> SignedRPC -> Either String Bool
verifySignedRPC KeySet{..} s@(SignedRPC Digest{..} bdy)
  | _digType == CMD || _digType == REV =
      case Map.lookup _digNodeId _ksClient of
        Nothing -> Left $ "PubKey not found for NodeID: " ++ show _digNodeId
        Just key
          | key /= _digPubkey -> Left $ "Public key in storage doesn't match digest's key for msg: " ++ show s
          | otherwise -> if not $ dverify key bdy _digSig
                         then Left $ "Unable to verify SignedRPC sig: " ++ show s
                         else Right True
  | otherwise =
      case Map.lookup _digNodeId _ksCluster of
        Nothing -> Left $ "PubKey not found for NodeID: " ++ show _digNodeId
        Just key
          | key /= _digPubkey -> Left $ "Public key in storage doesn't match digest's key for msg: " ++ show s
          | otherwise -> if not $ dverify key bdy _digSig
                         then Left $ "Unable to verify SignedRPC sig: " ++ show s
                         else Right True
{-# INLINE verifySignedRPC #-}

class WireFormat a where
  toWire   :: NodeID -> PublicKey -> SecretKey -> a -> SignedRPC
  fromWire :: KeySet -> SignedRPC -> Either String a

data Command = Command
  { _cmdEntry      :: !CommandEntry
  , _cmdClientId   :: !NodeID
  , _cmdRequestId  :: !RequestId
  , _cmdProvenance :: !Provenance
  }
  deriving (Show, Eq, Generic)
instance Serialize Command -- again, for SQLite

data CMDWire = CMDWire (CommandEntry, NodeID, RequestId)
  deriving (Show, Generic)
instance Serialize CMDWire

instance WireFormat Command where
  toWire nid pubKey privKey Command{..} = case _cmdProvenance of
    NewMsg -> let bdy = S.encode $ CMDWire (_cmdEntry, _cmdClientId, _cmdRequestId)
                  sig = dsign privKey bdy
                  dig = Digest nid sig pubKey CMD
              in SignedRPC dig bdy
    ReceivedMsg{..} -> SignedRPC _pDig _pOrig
  fromWire ks s@(SignedRPC dig bdy) = case verifySignedRPC ks s of
    Left err -> Left err
    Right False -> error "Invariant Failure: verification came back as Right False"
    Right True -> if _digType dig /= CMD
      then error $ "Invariant Failure: attempting to decode " ++ show (_digType dig) ++ " with CMDWire instance"
      else case S.decode bdy of
        Left err -> Left $ "Failure to decode CMDWire: " ++ err
        Right (CMDWire (ce,nid,rid)) -> Right $ Command ce nid rid $ ReceivedMsg dig bdy
  {-# INLINE toWire #-}
  {-# INLINE fromWire #-}

data CommandResponse = CommandResponse
  { _cmdrResult     :: !CommandResult
  , _cmdrLeaderId   :: !NodeID
  , _cmdrNodeId     :: !NodeID
  , _cmdrRequestId  :: !RequestId
  , _cmdrProvenance :: !Provenance
  }
  deriving (Show, Eq, Generic)

data CMDRWire = CMDRWire (CommandResult, NodeID, NodeID, RequestId)
  deriving (Show, Generic)
instance Serialize CMDRWire

instance WireFormat CommandResponse where
  toWire nid pubKey privKey CommandResponse{..} = case _cmdrProvenance of
    NewMsg -> let bdy = S.encode $ CMDRWire (_cmdrResult,_cmdrLeaderId,_cmdrNodeId,_cmdrRequestId)
                  sig = dsign privKey bdy
                  dig = Digest nid sig pubKey CMDR
              in SignedRPC dig bdy
    ReceivedMsg{..} -> SignedRPC _pDig _pOrig
  fromWire ks s@(SignedRPC dig bdy) = case verifySignedRPC ks s of
    Left err -> Left err
    Right False -> error "Invariant Failure: verification came back as Right False"
    Right True -> if _digType dig /= CMDR
      then error $ "Invariant Failure: attempting to decode " ++ show (_digType dig) ++ " with CMDRWire instance"
      else case S.decode bdy of
        Left err -> Left $ "Failure to decode CMDRWire: " ++ err
        Right (CMDRWire (r,lid,nid,rid)) -> Right $ CommandResponse r lid nid rid $ ReceivedMsg dig bdy
  {-# INLINE toWire #-}
  {-# INLINE fromWire #-}

data LogEntry = LogEntry
  { _leTerm    :: !Term
  , _leCommand :: !Command
  , _leHash    :: !ByteString
  }
  deriving (Show, Eq, Generic)
instance Serialize LogEntry -- again, for SQLite

data LEWire = LEWire (Term, SignedRPC, ByteString)
  deriving (Show, Generic)
instance Serialize LEWire

decodeLEWire :: KeySet -> [LEWire] -> Either String (Seq LogEntry)
decodeLEWire ks les = go les Seq.empty
  where
    go [] s = Right s
    go (LEWire (t,cmd,hsh):ls) v = case fromWire ks cmd of
      Left err -> Left err
      Right cmd' -> go ls (v |> LogEntry t cmd' hsh)
{-# INLINE decodeLEWire #-}

encodeLEWire :: NodeID -> PublicKey -> SecretKey -> Seq LogEntry -> [LEWire]
encodeLEWire nid pubKey privKey les =
  (\LogEntry{..} -> LEWire (_leTerm, toWire nid pubKey privKey _leCommand, _leHash)) <$> toList les
{-# INLINE encodeLEWire #-}

data AppendEntries = AppendEntries
  { _aeTerm        :: !Term
  , _leaderId      :: !NodeID
  , _prevLogIndex  :: !LogIndex
  , _prevLogTerm   :: !Term
  , _aeEntries     :: !(Seq LogEntry)
  , _aeQuorumVotes :: !(Set RequestVoteResponse)
  , _aeProvenance  :: !Provenance
  }
  deriving (Show, Eq, Generic)

data AEWire = AEWire (Term,NodeID,LogIndex,Term,[LEWire],[SignedRPC])
  deriving (Show, Generic)
instance Serialize AEWire

instance WireFormat AppendEntries where
  toWire nid pubKey privKey AppendEntries{..} = case _aeProvenance of
    NewMsg -> let bdy = S.encode $ AEWire (_aeTerm
                                              ,_leaderId
                                              ,_prevLogIndex
                                              ,_prevLogTerm
                                              ,encodeLEWire nid pubKey privKey _aeEntries
                                              ,toWire nid pubKey privKey <$> toList _aeQuorumVotes)
                  sig = dsign privKey bdy
                  dig = Digest nid sig pubKey AE
              in SignedRPC dig bdy
    ReceivedMsg{..} -> SignedRPC _pDig _pOrig
  fromWire ks s@(SignedRPC dig bdy) = case verifySignedRPC ks s of
    Left err -> Left err
    Right False -> error "Invariant Failure: verification came back as Right False"
    Right True -> if _digType dig /= AE
      then error $ "Invariant Failure: attempting to decode " ++ show (_digType dig) ++ " with AEWire instance"
      else case S.decode bdy of
        Left err -> Left $ "Failure to decode AEWire: " ++ err
        Right (AEWire (t,lid,pli,pt,les,vts)) -> case decodeLEWire ks les of
          Left err -> Left $ "Found a LogEntry with an invalid Command: " ++ err
          Right les' -> case decodeRVRWire ks vts of
            Left err -> Left $ "Caught an invalid RVR in an AE: " ++ err
            Right vts' -> Right $ AppendEntries t lid pli pt les' vts' $ ReceivedMsg dig bdy
  {-# INLINE toWire #-}
  {-# INLINE fromWire #-}

data AppendEntriesResponse = AppendEntriesResponse
  { _aerTerm       :: !Term
  , _aerNodeId     :: !NodeID
  , _aerSuccess    :: !Bool
  , _aerConvinced  :: !Bool
  , _aerIndex      :: !LogIndex
  , _aerHash       :: !ByteString
  , _aerProvenance :: !Provenance
  }
  deriving (Show, Generic, Eq, Ord)

data AERWire = AERWire (Term,NodeID,Bool,Bool,LogIndex,ByteString)
  deriving (Show, Generic)
instance Serialize AERWire

instance WireFormat AppendEntriesResponse where
  toWire nid pubKey privKey AppendEntriesResponse{..} = case _aerProvenance of
    NewMsg -> let bdy = S.encode $ AERWire ( _aerTerm
                                               , _aerNodeId
                                               , _aerSuccess
                                               , _aerConvinced
                                               , _aerIndex
                                               , _aerHash)
                  sig = dsign privKey bdy
                  dig = Digest nid sig pubKey AER
              in SignedRPC dig bdy
    ReceivedMsg{..} -> SignedRPC _pDig _pOrig
  fromWire ks s@(SignedRPC dig bdy) = case verifySignedRPC ks s of
    Left err -> Left err
    Right False -> error "Invariant Failure: verification came back as Right False"
    Right True -> if _digType dig /= AER
      then error $ "Invariant Failure: attempting to decode " ++ show (_digType dig) ++ " with AERWire instance"
      else case S.decode bdy of
        Left err -> Left $ "Failure to decode AERWire: " ++ err
        Right (AERWire (t,nid,s',c,i,h)) -> Right $ AppendEntriesResponse t nid s' c i h $ ReceivedMsg dig bdy
  {-# INLINE toWire #-}
  {-# INLINE fromWire #-}

data RequestVote = RequestVote
  { _rvTerm        :: !Term
  , _rvCandidateId :: !NodeID -- Sender ID Right? We don't forward RV's
  , _lastLogIndex  :: !LogIndex
  , _lastLogTerm   :: !Term
  , _rvProvenance  :: !Provenance
  }
  deriving (Show, Eq, Generic)

data RVWire = RVWire (Term,NodeID,LogIndex,Term)
  deriving (Show, Generic)
instance Serialize RVWire

instance WireFormat RequestVote where
  toWire nid pubKey privKey RequestVote{..} = case _rvProvenance of
    NewMsg -> let bdy = S.encode $ RVWire ( _rvTerm
                                              , _rvCandidateId
                                              , _lastLogIndex
                                              , _lastLogTerm)
                  sig = dsign privKey bdy
                  dig = Digest nid sig pubKey RV
              in SignedRPC dig bdy
    ReceivedMsg{..} -> SignedRPC _pDig _pOrig
  fromWire ks s@(SignedRPC dig bdy) = case verifySignedRPC ks s of
    Left err -> Left err
    Right False -> error "Invariant Failure: verification came back as Right False"
    Right True -> if _digType dig /= RV
      then error $ "Invariant Failure: attempting to decode " ++ show (_digType dig) ++ " with RVWire instance"
      else case S.decode bdy of
        Left err -> Left $ "Failure to decode RVWire: " ++ err
        Right (RVWire (t,cid,lli,llt)) -> Right $ RequestVote t cid lli llt $ ReceivedMsg dig bdy
  {-# INLINE toWire #-}
  {-# INLINE fromWire #-}

data RequestVoteResponse = RequestVoteResponse
  { _rvrTerm        :: !Term
  , _rvrCurLogIndex :: !LogIndex
  , _rvrNodeId      :: !NodeID
  , _voteGranted    :: !Bool
  , _rvrCandidateId :: !NodeID
  , _rvrProvenance  :: !Provenance
  }
  deriving (Show, Eq, Ord, Generic)
instance Serialize RequestVoteResponse -- again, for SQLite

data RVRWire = RVRWire (Term,LogIndex,NodeID,Bool,NodeID)
  deriving (Show, Generic)
instance Serialize RVRWire

instance WireFormat RequestVoteResponse where
  toWire nid pubKey privKey RequestVoteResponse{..} = case _rvrProvenance of
    NewMsg -> let bdy = S.encode $ RVRWire (_rvrTerm,_rvrCurLogIndex,_rvrNodeId,_voteGranted,_rvrCandidateId)
                  sig = dsign privKey bdy
                  dig = Digest nid sig pubKey RVR
              in SignedRPC dig bdy
    ReceivedMsg{..} -> SignedRPC _pDig _pOrig
  fromWire ks s@(SignedRPC dig bdy) = case verifySignedRPC ks s of
    Left err -> Left err
    Right False -> error "Invariant Failure: verification came back as Right False"
    Right True -> if _digType dig /= RVR
      then error $ "Invariant Failure: attempting to decode " ++ show (_digType dig) ++ " with RVRWire instance"
      else case S.decode bdy of
        Left err -> Left $ "Failure to decode RVRWire: " ++ err
        Right (RVRWire (t,li,nid,granted,cid)) -> Right $ RequestVoteResponse t li nid granted cid $ ReceivedMsg dig bdy
  {-# INLINE toWire #-}
  {-# INLINE fromWire #-}

-- the expected behavior here is tricky. For a set of votes, we are actually okay if some are invalid so long as there's a quorum
-- however while we're still in alpha I think these failures represent a bug. Hence, they should be raised asap.
decodeRVRWire :: KeySet -> [SignedRPC] -> Either String (Set RequestVoteResponse)
decodeRVRWire ks votes' = go votes' Set.empty
  where
    go [] s = Right s
    go (v:vs) s = case fromWire ks v of
      Left err -> Left err
      Right rvr' -> go vs (Set.insert rvr' s)
{-# INLINE decodeRVRWire #-}

data Revolution = Revolution
  { _revClientId   :: !NodeID
  , _revLeaderId   :: !NodeID
  , _revRequestId  :: !RequestId
  , _revProvenance :: !Provenance
  }
  deriving (Show, Eq, Generic)

data REVWire = REVWire (NodeID,NodeID,RequestId)
  deriving (Show, Generic)
instance Serialize REVWire

instance WireFormat Revolution where
  toWire nid pubKey privKey Revolution{..} = case _revProvenance of
    NewMsg -> let bdy = S.encode $ REVWire (_revClientId,_revLeaderId,_revRequestId)
                  sig = dsign privKey bdy
                  dig = Digest nid sig pubKey REV
              in SignedRPC dig bdy
    ReceivedMsg{..} -> SignedRPC _pDig _pOrig
  fromWire ks s@(SignedRPC dig bdy) = case verifySignedRPC ks s of
    Left err -> Left err
    Right False -> error "Invariant Failure: verification came back as Right False"
    Right True -> if _digType dig /= REV
      then error $ "Invariant Failure: attempting to decode " ++ show (_digType dig) ++ " with REVWire instance"
      else case S.decode bdy of
        Left err -> Left $ "Failure to decode REVWire: " ++ err
        Right (REVWire (cid,lid,rid)) -> Right $ Revolution cid lid rid $ ReceivedMsg dig bdy
  {-# INLINE toWire #-}
  {-# INLINE fromWire #-}

data RPC = AE'   AppendEntries
         | AER'  AppendEntriesResponse
         | RV'   RequestVote
         | RVR'  RequestVoteResponse
         | CMD'  Command
         | CMDR' CommandResponse
         | REV'  Revolution
  deriving (Show, Generic)

signedRPCtoRPC :: KeySet -> SignedRPC -> Either String RPC
signedRPCtoRPC ks s@(SignedRPC (Digest _ _ _ AE)   _) = AE'   <$> fromWire ks s
signedRPCtoRPC ks s@(SignedRPC (Digest _ _ _ AER)  _) = AER'  <$> fromWire ks s
signedRPCtoRPC ks s@(SignedRPC (Digest _ _ _ RV)   _) = RV'   <$> fromWire ks s
signedRPCtoRPC ks s@(SignedRPC (Digest _ _ _ RVR)  _) = RVR'  <$> fromWire ks s
signedRPCtoRPC ks s@(SignedRPC (Digest _ _ _ CMD)  _) = CMD'  <$> fromWire ks s
signedRPCtoRPC ks s@(SignedRPC (Digest _ _ _ CMDR) _) = CMDR' <$> fromWire ks s
signedRPCtoRPC ks s@(SignedRPC (Digest _ _ _ REV)  _) = REV'  <$> fromWire ks s
{-# INLINE signedRPCtoRPC #-}

rpcToSignedRPC :: NodeID -> PublicKey -> SecretKey -> RPC -> SignedRPC
rpcToSignedRPC nid pubKey privKey (AE' v) = toWire nid pubKey privKey v
rpcToSignedRPC nid pubKey privKey (AER' v) = toWire nid pubKey privKey v
rpcToSignedRPC nid pubKey privKey (RV' v) = toWire nid pubKey privKey v
rpcToSignedRPC nid pubKey privKey (RVR' v) = toWire nid pubKey privKey v
rpcToSignedRPC nid pubKey privKey (CMD' v) = toWire nid pubKey privKey v
rpcToSignedRPC nid pubKey privKey (CMDR' v) = toWire nid pubKey privKey v
rpcToSignedRPC nid pubKey privKey (REV' v) = toWire nid pubKey privKey v
{-# INLINE rpcToSignedRPC #-}

data Event = ERPC RPC
           | ElectionTimeout String
           | HeartbeatTimeout String
  deriving (Show)

data CommandStatus = CmdSubmitted -- client sets when sending command
                   | CmdAccepted  -- Raft client has recieved command and submitted
                   | CmdCommitted -- Raft has Committed the command, not yet applied
                   | CmdApplied { result :: CommandResult }  -- We have a result
                   deriving (Show)

-- | A structure containing all the implementation details for running
-- the raft protocol.
-- Types:
-- nt -- "node type", ie identifier (host/port, topic, subject)
-- et -- "entry type", serialized format for submissions into Raft
-- rt -- "return type", serialized format for "results" or "responses"
-- mt -- "message type", serialized format for sending over wire
data RaftSpec m = RaftSpec
  {
    -- ^ Function to get a log entry from persistent storage.
    _readLogEntry     :: LogIndex -> m (Maybe CommandEntry) -- Simple [unused]

    -- ^ Function to write a log entry to persistent storage.
  , _writeLogEntry    :: LogIndex -> (Term,CommandEntry) -> m () -- Simple [unused]

    -- ^ Function to get the term number from persistent storage.
  , _readTermNumber   :: m Term -- Simple [unused]

    -- ^ Function to write the term number to persistent storage.
  , _writeTermNumber  :: Term -> m () -- Simple,Util(updateTerm[write only])

    -- ^ Function to read the node voted for from persistent storage.
  , _readVotedFor     :: m (Maybe NodeID) -- Simple [unused]

    -- ^ Function to write the node voted for to persistent storage.
  , _writeVotedFor    :: Maybe NodeID -> m () -- Simple,Role [write only]

    -- ^ Function to apply a log entry to the state machine.
  , _applyLogEntry    :: CommandEntry -> m CommandResult -- Simple,Handler

    -- ^ Function to send a message to a node.
  , _sendMessage      :: NodeID -> ByteString -> m () -- Simple,Sender

    -- ^ Function to get the next message.
  , _getMessage       :: m ByteString -- Simple,Util(messageReceiver)

    -- ^ Function to log a debug message (no newline).
  , _debugPrint       :: NodeID -> String -> m () -- Simple,Util(debug)

  , _random           :: forall a . Random a => (a, a) -> m a -- Simple,Util(randomRIO[timer])

  , _enqueue          :: Event -> m () -- Simple,Util(enqueueEvent)

  , _enqueueLater     :: Int -> Event -> m ThreadId -- Simple,Util(enqueueEventLater[timer])

  , _killEnqueued     :: ThreadId -> m () -- Simple,Timer

  , _dequeue          :: m Event -- Simple,Util(dequeueEvent)

  }
makeLenses (''RaftSpec)

data Role = Follower
          | Candidate
          | Leader
  deriving (Show, Generic, Eq)

data RaftState = RaftState
  { _role             :: Role -- Handler,Role,Util(debug)
  , _term             :: Term -- Handler,Role,Sender,Util(updateTerm)
  , _votedFor         :: Maybe NodeID -- Handler,Role
  , _lazyVote         :: Maybe (Term, NodeID, LogIndex) -- Handler
  , _currentLeader    :: Maybe NodeID -- Client,Handler,Role
  , _ignoreLeader     :: Bool -- Handler
  , _logEntries       :: Seq LogEntry -- Handler,Role,Sender
  , _commitIndex      :: LogIndex -- Handler
  , _lastApplied      :: LogIndex -- Handler
  , _commitProof      :: Map LogIndex (Set AppendEntriesResponse) -- Handler
  , _timerThread      :: Maybe ThreadId -- Timer
  , _timeSinceLastAER :: Int -- microseconds
  , _replayMap        :: Map (NodeID, Signature) (Maybe CommandResult) -- Handler
  , _cYesVotes        :: Set RequestVoteResponse -- Handler,Role,Sender
  , _cPotentialVotes  :: Set NodeID -- Hander,Role,Sender
  , _lNextIndex       :: Map NodeID LogIndex -- Handler,Role,Sender
  , _lMatchIndex      :: Map NodeID LogIndex -- Role (never read?)
  , _lConvinced       :: Set NodeID -- Handler,Role,Sender

  -- used by clients
  , _pendingRequests  :: Map RequestId Command -- Client
  , _currentRequestId :: RequestId -- Client
  , _numTimeouts      :: Int -- Client
  }
makeLenses ''RaftState

initialRaftState :: RaftState
initialRaftState = RaftState
  Follower   -- role
  startTerm  -- term
  Nothing    -- votedFor
  Nothing    -- lazyVote
  Nothing    -- currentLeader
  False      -- ignoreLeader
  Seq.empty  -- log
  startIndex -- commitIndex
  startIndex -- lastApplied
  Map.empty  -- commitProof
  Nothing    -- timerThread
  0          -- timeSinceLastAER
  Map.empty  -- replayMap
  Set.empty  -- cYesVotes
  Set.empty  -- cPotentialVotes
  Map.empty  -- lNextIndex
  Map.empty  -- lMatchIndex
  Set.empty  -- lConvinced
  Map.empty  -- pendingRequests
  0          -- nextRequestId
  0          -- numTimeouts


type Raft m = RWST (RaftEnv m) () RaftState m

data RaftEnv m = RaftEnv
  { _cfg        :: Config
  , _quorumSize :: Int  -- Handler,Role
  , _rs         :: RaftSpec (Raft m)
  }
makeLenses ''RaftEnv
