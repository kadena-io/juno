{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Juno.Types.Message.CMD
  ( Command(..), cmdEntry, cmdClientId, cmdRequestId, cmdProvenance
  , CommandBatch(..), cmdbBatch, cmdbProvenance
  , gatherValidCmdbs
  ) where

import Control.Parallel.Strategies
import Control.Lens
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Data.Thyme.Time.Core ()
import GHC.Generics

import Juno.Types.Base
import Juno.Types.Command
import Juno.Types.Message.Signed

data Command = Command
  { _cmdEntry      :: !CommandEntry
  , _cmdClientId   :: !NodeID
  , _cmdRequestId  :: !RequestId
  , _cmdProvenance :: !Provenance
  }
  deriving (Show, Eq, Generic)
makeLenses ''Command

data CMDWire = CMDWire !(CommandEntry, NodeID, RequestId)
  deriving (Show, Generic)
instance Serialize CMDWire

instance WireFormat Command where
  toWire nid pubKey privKey Command{..} = case _cmdProvenance of
    NewMsg -> let bdy = S.encode $ CMDWire (_cmdEntry, _cmdClientId, _cmdRequestId)
                  sig = sign bdy privKey pubKey
                  dig = Digest nid sig pubKey CMD
              in SignedRPC dig bdy
    ReceivedMsg{..} -> SignedRPC _pDig _pOrig
  fromWire !ts !ks s@(SignedRPC !dig !bdy) =
    case verifySignedRPC ks s of
      Left !err -> Left err
      Right () -> if _digType dig /= CMD
        then error $ "Invariant Failure: attempting to decode " ++ show (_digType dig) ++ " with CMDWire instance"
        else case S.decode bdy of
            Left !err -> Left $! "Failure to decode CMDWire: " ++ err
            Right (CMDWire !(ce,nid,rid)) -> Right $! Command ce nid rid $ ReceivedMsg dig bdy ts
  {-# INLINE toWire #-}
  {-# INLINE fromWire #-}

data CommandBatch = CommandBatch
  { _cmdbBatch :: ![Command]
  , _cmdbProvenance :: !Provenance
  } deriving (Show, Eq, Generic)
makeLenses ''CommandBatch

instance WireFormat CommandBatch where
  toWire nid pubKey privKey CommandBatch{..} = case _cmdbProvenance of
    NewMsg -> let bdy = S.encode ((toWire nid pubKey privKey <$> _cmdbBatch) `using` parList rseq)
                  sig = sign bdy privKey pubKey
                  dig = Digest nid sig pubKey CMDB
              in SignedRPC dig bdy
    ReceivedMsg{..} -> SignedRPC _pDig _pOrig
  fromWire !ts !ks s@(SignedRPC dig bdy) = case verifySignedRPC ks s of
    Left !err -> Left err
    Right () -> if _digType dig /= CMDB
      then error $! "Invariant Failure: attempting to decode " ++ show (_digType dig) ++ " with CMDBWire instance"
      else case S.decode bdy of
        Left !err -> Left $ "Failure to decode CMDBWire: " ++ err
        Right !cmdb' -> gatherValidCmdbs (ReceivedMsg dig bdy ts) ((fromWire ts ks <$> cmdb') `using` parList rseq)
  {-# INLINE toWire #-}
  {-# INLINE fromWire #-}

gatherValidCmdbs :: Provenance -> [Either String Command] -> Either String CommandBatch
gatherValidCmdbs prov ec = (`CommandBatch` prov) <$> sequence ec
{-# INLINE gatherValidCmdbs #-}
