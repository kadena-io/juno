{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Juno.Types.Base
  ( NodeID(..)
  , Term(..), startTerm
  , LogIndex(..), startIndex
  , RequestId(..), startRequestId, toRequestId
  , ReceivedAt(..)
  -- for simplicity, re-export some core types that we need all over the place
  , PublicKey, PrivateKey, Signature(..), sign, valid, importPublic, importPrivate
  , Role(..)
  ) where

import Control.Monad (mzero)
import Crypto.Ed25519.Pure ( PublicKey, PrivateKey, Signature(..), sign, valid
                           , importPublic, importPrivate, exportPublic, exportPrivate)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Data.Word (Word64)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Thyme.Clock
import Data.Thyme.Time.Core ()
import Data.Thyme.Internal.Micro (Micro)
import Data.Aeson (genericParseJSON,genericToJSON,parseJSON,toJSON,ToJSON,FromJSON,Value(..))
import Data.Aeson.Types (defaultOptions,Options(..))

import GHC.Int (Int64)
import GHC.Generics hiding (from)

data NodeID = NodeID { _host :: !String, _port :: !Word64, _fullAddr :: !String }
  deriving (Eq,Ord,Read,Show,Generic)
instance Serialize NodeID
instance ToJSON NodeID where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }
instance FromJSON NodeID where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

newtype Term = Term Int
  deriving (Show, Read, Eq, Enum, Num, Ord, Generic, Serialize)

startTerm :: Term
startTerm = Term (-1)

newtype LogIndex = LogIndex Int
  deriving (Show, Read, Eq, Ord, Enum, Num, Real, Integral, Generic, Serialize)

startIndex :: LogIndex
startIndex = LogIndex (-1)

newtype RequestId = RequestId Int64
  deriving (Show, Read, Eq, Ord, Enum, Num, Generic, Serialize)

startRequestId :: RequestId
startRequestId = RequestId 0

toRequestId :: Int64 -> RequestId
toRequestId a = RequestId a

deriving instance Eq Signature
deriving instance Ord Signature
instance Serialize Signature where
  put (Sig s) = S.put s
  get = Sig <$> (S.get >>= S.getByteString)

instance Eq PublicKey where
  b == b' = exportPublic b == exportPublic b'
instance Ord PublicKey where
  b <= b' = exportPublic b <= exportPublic b'
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
  b <= b' = exportPrivate b <= exportPrivate b'
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

data Role = Follower
          | Candidate
          | Leader
  deriving (Show, Generic, Eq)
