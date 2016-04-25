{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Juno.Types.Message.CMDR
  ( CommandResponse(..), cmdrResult, cmdrLeaderId, cmdrNodeId, cmdrRequestId, cmdrProvenance
  ) where

import Control.Lens
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Data.Thyme.Time.Core ()
import GHC.Generics

import Juno.Types.Base
import Juno.Types.Command
import Juno.Types.Message.Signed

data CommandResponse = CommandResponse
  { _cmdrResult     :: !CommandResult
  , _cmdrLeaderId   :: !NodeID
  , _cmdrNodeId     :: !NodeID
  , _cmdrRequestId  :: !RequestId
  , _cmdrProvenance :: !Provenance
  }
  deriving (Show, Eq, Generic)
makeLenses ''CommandResponse

data CMDRWire = CMDRWire (CommandResult, NodeID, NodeID, RequestId)
  deriving (Show, Generic)
instance Serialize CMDRWire

instance WireFormat CommandResponse where
  toWire nid pubKey privKey CommandResponse{..} = case _cmdrProvenance of
    NewMsg -> let bdy = S.encode $ CMDRWire (_cmdrResult,_cmdrLeaderId,_cmdrNodeId,_cmdrRequestId)
                  sig = sign bdy privKey pubKey
                  dig = Digest nid sig pubKey CMDR
              in SignedRPC dig bdy
    ReceivedMsg{..} -> SignedRPC _pDig _pOrig
  fromWire ts !ks s@(SignedRPC !dig !bdy) = case verifySignedRPC ks s of
    Left !err -> Left err
    Right () -> if _digType dig /= CMDR
      then error $ "Invariant Failure: attempting to decode " ++ show (_digType dig) ++ " with CMDRWire instance"
      else case S.decode bdy of
        Left !err -> Left $! "Failure to decode CMDRWire: " ++ err
        Right (CMDRWire !(r,lid,nid,rid)) -> Right $! CommandResponse r lid nid rid $ ReceivedMsg dig bdy ts
  {-# INLINE toWire #-}
  {-# INLINE fromWire #-}
