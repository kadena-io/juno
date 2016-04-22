{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Juno.Types.Message.AER
  ( AppendEntriesResponse(..), aerTerm, aerNodeId, aerSuccess, aerConvinced
  , aerIndex, aerHash, aerWasVerified, aerProvenance
  , AERWire(..)
  , AlotOfAERs(..), unAlot
  ) where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.ByteString (ByteString)
import Data.Serialize (Serialize)
import qualified Data.Serialize as S
import Data.Thyme.Time.Core ()
import GHC.Generics

import Juno.Types.Base
import Juno.Types.Message.Signed

data AppendEntriesResponse = AppendEntriesResponse
  { _aerTerm       :: !Term
  , _aerNodeId     :: !NodeID
  , _aerSuccess    :: !Bool
  , _aerConvinced  :: !Bool
  , _aerIndex      :: !LogIndex
  , _aerHash       :: !ByteString
  , _aerWasVerified:: !Bool
  , _aerProvenance :: !Provenance
  }
  deriving (Show, Generic, Eq)
makeLenses ''AppendEntriesResponse

instance Ord AppendEntriesResponse where
  -- This is here to get a set of AERs to order correctly
  -- Node matters most (apples to apples)
  -- Term supersedes index always
  -- Index is really what we're after for how Set AER is used
  -- Hash matters more than verified due to conflict resolution
  -- Then verified, which is metadata really, because if everything up to that point is the same then the one that already ran through crypto is more valuable
  -- After that it doesn't matter.
  (AppendEntriesResponse t n s c i h v p) <= (AppendEntriesResponse t' n' s' c' i' h' v' p') =
    (n,t,i,h,v,s,c,p) <= (n',t',i',h',v',s',c',p')

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
                  sig = sign bdy privKey pubKey
                  dig = Digest nid sig pubKey AER
              in SignedRPC dig bdy
    ReceivedMsg{..} -> SignedRPC _pDig _pOrig
  fromWire !ts !ks s@(SignedRPC !dig !bdy) = case verifySignedRPC ks s of
    Left !err -> Left $! err
    Right () -> if _digType dig /= AER
      then error $ "Invariant Failure: attempting to decode " ++ show (_digType dig) ++ " with AERWire instance"
      else case S.decode bdy of
        Left !err -> Left $! "Failure to decode AERWire: " ++ err
        Right (AERWire !(t,nid,s',c,i,h)) -> Right $! AppendEntriesResponse t nid s' c i h True $ ReceivedMsg dig bdy ts
  {-# INLINE toWire #-}
  {-# INLINE fromWire #-}


newtype AlotOfAERs = AlotOfAERs { _unAlot :: Map NodeID (Set AppendEntriesResponse)}
  deriving (Show, Eq)
makeLenses ''AlotOfAERs

instance Monoid AlotOfAERs where
  mempty = AlotOfAERs Map.empty
  mappend (AlotOfAERs m) (AlotOfAERs m') = AlotOfAERs $ Map.unionWith Set.union m m'
