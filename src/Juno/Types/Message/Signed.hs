{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Juno.Types.Message.Signed
  ( MsgType(..)
  , Provenance(..)
  , Digest(..), digNodeId, digSig, digPubkey, digType
  , SignedRPC(..)
  -- for testing & benchmarks
  , verifySignedRPC
  , WireFormat(..)
  ) where

import Control.Lens hiding (Index, (|>))
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import Data.Serialize (Serialize)
import Data.Thyme.Time.Core ()
import GHC.Generics hiding (from)

import Juno.Types.Base
import Juno.Types.Config

-- | One way or another we need a way to figure our what set of public keys to use for verification of signatures.
-- By placing the message type in the digest, we can make the WireFormat implementation easier as well. CMD and REV
-- need to use the Client Public Key maps.
data MsgType = AE | AER | RV | RVR | CMD | CMDR | CMDB | REV
  deriving (Show, Eq, Ord, Generic)
instance Serialize MsgType

-- | Digest containing Sender ID, Signature, Sender's Public Key and the message type
data Digest = Digest
  { _digNodeId :: !NodeID
  , _digSig    :: !Signature
  , _digPubkey :: !PublicKey
  , _digType   :: !MsgType
  } deriving (Show, Eq, Ord, Generic)
makeLenses ''Digest

instance Serialize Digest

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

-- | Type that is serialized and sent over the wire
data SignedRPC = SignedRPC
  { _sigDigest :: !Digest
  , _sigBody   :: !ByteString
  } deriving (Show, Eq, Generic)
instance Serialize SignedRPC

class WireFormat a where
  toWire   :: NodeID -> PublicKey -> PrivateKey -> a -> SignedRPC
  fromWire :: Maybe ReceivedAt -> KeySet -> SignedRPC -> Either String a

-- | Based on the MsgType in the SignedRPC's Digest, we know which set of keys are needed to validate the message
verifySignedRPC :: KeySet -> SignedRPC -> Either String ()
verifySignedRPC !KeySet{..} s@(SignedRPC !Digest{..} !bdy)
  | _digType == CMD || _digType == REV || _digType == CMDB =
      case Map.lookup _digNodeId _ksClient of
        Nothing -> Left $! "PubKey not found for NodeID: " ++ show _digNodeId
        Just !key
          | key /= _digPubkey -> Left $! "Public key in storage doesn't match digest's key for msg: " ++ show s
          | otherwise -> if not $ valid bdy key _digSig
                         then Left $! "Unable to verify SignedRPC sig: " ++ show s
                         else Right ()
  | otherwise =
      case Map.lookup _digNodeId _ksCluster of
        Nothing -> Left $! "PubKey not found for NodeID: " ++ show _digNodeId
        Just !key
          | key /= _digPubkey -> Left $! "Public key in storage doesn't match digest's key for msg: " ++ show s
          | otherwise -> if not $ valid bdy key _digSig
                         then Left $! "Unable to verify SignedRPC sig: " ++ show s
                         else Right ()
{-# INLINE verifySignedRPC #-}
