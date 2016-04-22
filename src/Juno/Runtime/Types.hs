{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Juno.Runtime.Types
--
-- Share types used in Api/Types.hs and Protocol/Types.
-- Api is responisble for accepting outside commands
-- Protocol is part of BFT raft implementation.

module Juno.Runtime.Types
  ( NodeID(..)
  , CommandEntry(..)
  , CommandResult(..)
  , CommandStatus(..)
  , CommandMap(..), CommandMVarMap, initCommandMap, setNextCmdRequestId
  , RequestId(..), startRequestId, toRequestId
  , Command(..)
  , CommandResponse(..)
  , CommandBatch(..)
  , MsgType(..), Digest(..), Provenance(..)
  , ReceivedAt(..)
  -- for simplicity, re-export some core types that we need all over the place
  , PublicKey, PrivateKey, Signature(..), sign, valid, importPublic, importPrivate
  ) where

import Control.Monad (mzero)
import Control.Concurrent (MVar, newMVar, takeMVar, putMVar)
import Crypto.Ed25519.Pure ( PublicKey, PrivateKey, Signature(..), sign, valid
                           , importPublic, importPrivate, exportPublic, exportPrivate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Data.Word (Word64)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Thyme.Clock
import Data.Thyme.Time.Core ()
import Data.Thyme.Time.Core (unUTCTime, toMicroseconds)
import Data.Thyme.Internal.Micro (Micro)
import Data.Aeson (genericParseJSON,genericToJSON,parseJSON,toJSON,ToJSON,FromJSON,Value(..))
import Data.Aeson.Types (defaultOptions,Options(..))

import GHC.Int (Int64)
import GHC.Generics hiding (from)

-- Shared between API and Juno protocol
-- holds the command result when the status is CmdApplied
data CommandMap = CommandMap
  { _cmvNextRequestId :: RequestId
  , _cmvMap :: Map RequestId CommandStatus
  } deriving (Show)

type CommandMVarMap = MVar CommandMap

-- If we initialize the request ID from zero every time, then when you restart the client the rid resets too.
-- We've hit bugs by doing this before. The hack we use is to initialize it to UTC Time
initCommandMap :: IO CommandMVarMap
initCommandMap = do
  UTCTime _ time <- unUTCTime <$> getCurrentTime
  newMVar $ CommandMap (RequestId $ toMicroseconds time) Map.empty

-- move to utils, this is the only CommandStatus that should inc the requestId
-- NB: this only works when we have a single client, but punting on solving this for now is a good idea.
setNextCmdRequestId :: CommandMVarMap -> IO RequestId
setNextCmdRequestId cmdStatusMap = do
  (CommandMap nextId m) <- takeMVar cmdStatusMap
  putMVar cmdStatusMap $ CommandMap (nextId + 1) (Map.insert nextId CmdSubmitted m)
  return nextId

newtype CommandEntry = CommandEntry { unCommandEntry :: ByteString }
  deriving (Show, Eq, Ord, Generic, Serialize)

newtype CommandResult = CommandResult { unCommandResult :: ByteString }
  deriving (Show, Eq, Ord, Generic, Serialize)

data NodeID = NodeID { _host :: !String, _port :: !Word64 }
  deriving (Eq,Ord,Read,Show,Generic)
instance Serialize NodeID
instance ToJSON NodeID where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }
instance FromJSON NodeID where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

newtype RequestId = RequestId Int64
  deriving (Show, Read, Eq, Ord, Enum, Num, Generic, Serialize)

startRequestId :: RequestId
startRequestId = RequestId 0

toRequestId :: Int64 -> RequestId
toRequestId a = RequestId a

instance Eq PublicKey where
  b == b' = exportPublic b == exportPublic b'
instance Ord PublicKey where
  b < b' = exportPublic b < exportPublic b'
  b <= b' = exportPublic b <= exportPublic b'
  b > b' = exportPublic b > exportPublic b'
  b >= b' = exportPublic b >= exportPublic b'
instance ToJSON PublicKey where
  toJSON = toJSON . decodeUtf8 . B16.encode . exportPublic
instance FromJSON PublicKey where
  parseJSON (String s) = do
    (s',leftovers) <- return $ B16.decode $ encodeUtf8 s
    if leftovers == B.empty
      then case importPublic s' of
             Just pk -> return pk
             Nothing -> mzero
      else mzero
  parseJSON _ = mzero
instance ToJSON (Map NodeID PublicKey) where
  toJSON = toJSON . Map.toList
instance FromJSON (Map NodeID PublicKey) where
  parseJSON = fmap Map.fromList . parseJSON
instance Eq PrivateKey where
  b == b' = exportPrivate b == exportPrivate b'
instance Ord PrivateKey where
  b < b' = exportPrivate b < exportPrivate b'
  b <= b' = exportPrivate b <= exportPrivate b'
  b > b' = exportPrivate b > exportPrivate b'
  b >= b' = exportPrivate b >= exportPrivate b'
instance ToJSON PrivateKey where
  toJSON = toJSON . decodeUtf8 . B16.encode . exportPrivate
instance FromJSON PrivateKey where
  parseJSON (String s) = do
    (s',leftovers) <- return $ B16.decode $ encodeUtf8 s
    if leftovers == B.empty
      then case importPrivate s' of
             Just pk -> return pk
             Nothing -> mzero
      else mzero
  parseJSON _ = mzero
instance ToJSON (Map NodeID PrivateKey) where
  toJSON = toJSON . Map.toList
instance FromJSON (Map NodeID PrivateKey) where
  parseJSON = fmap Map.fromList . parseJSON

-- | Digest containing Sender ID, Signature, Sender's Public Key and the message type
data Digest = Digest
  { _digNodeId :: !NodeID
  , _digSig    :: !Signature
  , _digPubkey :: !PublicKey
  , _digType   :: !MsgType
  } deriving (Show, Eq, Ord, Generic)
deriving instance Eq Signature
deriving instance Ord Signature
instance Serialize Signature where
  put (Sig s) = S.put s
  get = Sig <$> (S.get >>= S.getByteString)

instance Serialize Digest

-- These instances suck, but I can't figure out how to use the Get monad to fail out if not
-- length = 32. For the record, if the getByteString 32 works the imports will not fail
instance Serialize PublicKey where
  put s = S.putByteString (exportPublic s)
  get = maybe (error "Invalid PubKey") id . importPublic <$> S.getByteString (32::Int)
instance Serialize PrivateKey where
  put s = S.putByteString (exportPrivate s)
  get = maybe (error "Invalid PubKey") id . importPrivate <$> S.getByteString (32::Int)

-- | UTCTime from Thyme of when ZMQ received the message
newtype ReceivedAt = ReceivedAt {_unReceivedAt :: UTCTime}
  deriving (Show, Eq, Ord, Generic)
instance Serialize ReceivedAt
instance Serialize UTCTime
instance Serialize NominalDiffTime
instance Serialize Micro

-- | One way or another we need a way to figure our what set of public keys to use for verification of signatures.
-- By placing the message type in the digest, we can make the WireFormat implementation easier as well. CMD and REV
-- need to use the Client Public Key maps.
data MsgType = AE | AER | RV | RVR | CMD | CMDR | CMDB | REV
  deriving (Show, Eq, Ord, Generic)
instance Serialize MsgType

-- | Provenance is used to track if we made the message or received it. This is important for re-transmission.
data Provenance =
  NewMsg |
  ReceivedMsg
    { _pDig :: !Digest
    , _pOrig :: !ByteString
    , _pTimeStamp :: !(Maybe ReceivedAt)
    } deriving (Show, Eq, Ord, Generic)
-- instance Serialize Provenance <== This is bait, if you uncomment it you've done something wrong
-- We really want to be sure that Provenance from one Node isn't by accident transferred to another node.
-- Without a Serialize instance, we can be REALLY sure.

data Command = Command
  { _cmdEntry      :: !CommandEntry
  , _cmdClientId   :: !NodeID
  , _cmdRequestId  :: !RequestId
  , _cmdProvenance :: !Provenance
  }
  deriving (Show, Eq, Generic)

data CommandBatch = CommandBatch
  { _cmdbBatch :: ![Command]
  , _cmdbProvenance :: !Provenance
  } deriving (Show, Eq, Generic)

data CommandResponse = CommandResponse
  { _cmdrResult     :: !CommandResult
  , _cmdrLeaderId   :: !NodeID
  , _cmdrNodeId     :: !NodeID
  , _cmdrRequestId  :: !RequestId
  , _cmdrProvenance :: !Provenance
  }
  deriving (Show, Eq, Generic)

data CommandStatus = CmdSubmitted -- client sets when sending command
                   | CmdAccepted  -- Raft client has recieved command and submitted
                   | CmdApplied { result :: CommandResult }  -- We have a result
                   deriving (Show)
