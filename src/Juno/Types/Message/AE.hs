{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Juno.Types.Message.AE
  ( AppendEntries(..), aeTerm, leaderId, prevLogIndex, prevLogTerm, aeEntries, aeQuorumVotes, aeProvenance
  ) where

import Control.Parallel.Strategies
import Control.Lens
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Data.Foldable
import Data.Thyme.Time.Core ()
import GHC.Generics

import Juno.Types.Base
import Juno.Types.Message.Signed
import Juno.Types.Message.RVR
import Juno.Types.Log

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
makeLenses ''AppendEntries

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
                  sig = sign bdy privKey pubKey
                  dig = Digest nid sig pubKey AE
              in SignedRPC dig bdy
    ReceivedMsg{..} -> SignedRPC _pDig _pOrig
  fromWire !ts !ks s@(SignedRPC !dig !bdy) = case verifySignedRPC ks s of
    Left !err -> Left err
    Right () -> if _digType dig /= AE
      then error $ "Invariant Failure: attempting to decode " ++ show (_digType dig) ++ " with AEWire instance"
      else case S.decode bdy of
        Left err -> Left $! "Failure to decode AEWire: " ++ err
        Right (AEWire (t,lid,pli,pt,les,vts)) -> runEval $ do
          eLes <- rpar (toSeqLogEntry ((decodeLEWire' ts ks <$> les) `using` parList rseq))
          eRvr <- rseq (toSetRvr ((fromWire ts ks <$> vts) `using` parList rseq))
          case eRvr of
            Left !err -> return $! Left $! "Caught an invalid RVR in an AE: " ++ err
            Right !vts' -> do
              _ <- rseq eLes
              case eLes of
                Left !err -> return $! Left $ "Found a LogEntry with an invalid Command: " ++ err
                Right !les' -> return $! Right $! AppendEntries t lid pli pt les' vts' $ ReceivedMsg dig bdy ts
  {-# INLINE toWire #-}
  {-# INLINE fromWire #-}
