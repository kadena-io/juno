{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Juno.Types.Message.REV
  ( Revolution(..), revClientId, revLeaderId, revRequestId, revProvenance
  ) where

import Control.Lens
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Data.Thyme.Time.Core ()
import GHC.Generics

import Juno.Types.Base
import Juno.Types.Message.Signed

data Revolution = Revolution
  { _revClientId   :: !NodeID
  , _revLeaderId   :: !NodeID
  , _revRequestId  :: !RequestId
  , _revProvenance :: !Provenance
  }
  deriving (Show, Eq, Generic)
makeLenses ''Revolution

data REVWire = REVWire (NodeID,NodeID,RequestId)
  deriving (Show, Generic)
instance Serialize REVWire

instance WireFormat Revolution where
  toWire nid pubKey privKey Revolution{..} = case _revProvenance of
    NewMsg -> let bdy = S.encode $ REVWire (_revClientId,_revLeaderId,_revRequestId)
                  sig = sign bdy privKey pubKey
                  dig = Digest nid sig pubKey REV
              in SignedRPC dig bdy
    ReceivedMsg{..} -> SignedRPC _pDig _pOrig
  fromWire !ts !ks s@(SignedRPC !dig !bdy) = case verifySignedRPC ks s of
    Left !err -> Left $! err
    Right () -> if _digType dig /= REV
      then error $ "Invariant Failure: attempting to decode " ++ show (_digType dig) ++ " with REVWire instance"
      else case S.decode bdy of
        Left !err -> Left $! "Failure to decode REVWire: " ++ err
        Right (REVWire !(cid,lid,rid)) -> Right $! Revolution cid lid rid $ ReceivedMsg dig bdy ts
  {-# INLINE toWire #-}
  {-# INLINE fromWire #-}
