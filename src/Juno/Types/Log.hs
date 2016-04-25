{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Juno.Types.Log
  ( LogEntry(..), leTerm, leCommand, leHash
  , Log(..), lEntries
  , LEWire(..), encodeLEWire, decodeLEWire, decodeLEWire', toSeqLogEntry
  , lookupEntry
  , lastEntry
  , takeEntries
  , getEntriesAfter
  , logInfoForNextIndex
  , lastLogTerm
  , lastLogHash
  , entryCount
  , maxIndex
  , appendLogEntry
  , addLogEntriesAt
  ) where

import Control.Parallel.Strategies
import Control.Lens hiding (Index, (|>))
import Codec.Digest.SHA
import qualified Control.Lens as Lens
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.ByteString (ByteString)
import Data.Serialize
import Data.Foldable
import Data.Thyme.Time.Core ()
import GHC.Generics

import Juno.Types.Base
import Juno.Types.Config
import Juno.Types.Message.Signed
import Juno.Types.Message.CMD

data LogEntry = LogEntry
  { _leTerm    :: !Term
  , _leCommand :: !Command
  , _leHash    :: !ByteString
  }
  deriving (Show, Eq, Generic)
makeLenses ''LogEntry

newtype Log a = Log { _lEntries :: Seq a }
    deriving (Eq,Show,Ord,Generic,Monoid,Functor,Foldable,Traversable,Applicative,Monad,NFData)
makeLenses ''Log
instance (t ~ Log a) => Rewrapped (Log a) t
instance Wrapped (Log a) where
    type Unwrapped (Log a) = Seq a
    _Wrapped' = iso _lEntries Log
instance Cons (Log a) (Log a) a a where
    _Cons = _Wrapped . _Cons . mapping _Unwrapped
instance Snoc (Log a) (Log a) a a where
    _Snoc = _Wrapped . _Snoc . firsting _Unwrapped
type instance IxValue (Log a) = a
type instance Lens.Index (Log a) = LogIndex
instance Ixed (Log a) where ix i = lEntries.ix (fromIntegral i)

data LEWire = LEWire (Term, SignedRPC, ByteString)
  deriving (Show, Generic)
instance Serialize LEWire

decodeLEWire' :: Maybe ReceivedAt -> KeySet -> LEWire -> Either String LogEntry
decodeLEWire' !ts !ks (LEWire !(t,cmd,hsh)) = case fromWire ts ks cmd of
      Left !err -> Left $!err
      Right !cmd' -> Right $! LogEntry t cmd' hsh
{-# INLINE decodeLEWire' #-}

-- TODO: check if `toSeqLogEntry ele = Seq.fromList <$> sequence ele` is fusable?
toSeqLogEntry :: [Either String LogEntry] -> Either String (Seq LogEntry)
toSeqLogEntry !ele = go ele mempty
  where
    go [] s = Right $! s
    go (Right le:les) s = go les (s |> le)
    go (Left err:_) _ = Left $! err
{-# INLINE toSeqLogEntry #-}

decodeLEWire :: Maybe ReceivedAt -> KeySet -> [LEWire] -> Either String (Seq LogEntry)
decodeLEWire !ts !ks !les = go les Seq.empty
  where
    go [] s = Right $! s
    go (LEWire !(t,cmd,hsh):ls) v = case fromWire ts ks cmd of
      Left err -> Left $! err
      Right cmd' -> go ls (v |> LogEntry t cmd' hsh)
{-# INLINE decodeLEWire #-}

encodeLEWire :: NodeID -> PublicKey -> PrivateKey -> Seq LogEntry -> [LEWire]
encodeLEWire nid pubKey privKey les =
  (\LogEntry{..} -> LEWire (_leTerm, toWire nid pubKey privKey _leCommand, _leHash)) <$> toList les
{-# INLINE encodeLEWire #-}

-- | Get last entry.
lastEntry :: Log a -> Maybe a
lastEntry (_ :> e) = Just e
lastEntry _ = Nothing

-- | Get largest index in ledger.
maxIndex :: Log a -> LogIndex
maxIndex = subtract 1 . entryCount

-- | Get count of entries in ledger.
entryCount :: Log a -> LogIndex
entryCount = fromIntegral . Seq.length . view lEntries

-- | Safe index
lookupEntry :: LogIndex -> Log LogEntry -> Maybe LogEntry
lookupEntry i = firstOf (ix i)

-- | take operation
takeEntries :: LogIndex -> Log a -> Seq a
takeEntries t = Seq.take (fromIntegral t) . _lEntries

-- | called by leaders sending appendEntries.
-- given a replica's nextIndex, get the index and term to send as
-- prevLog(Index/Term)
logInfoForNextIndex :: Maybe LogIndex -> Log LogEntry -> (LogIndex,Term)
logInfoForNextIndex mni es =
  case mni of
    Just ni -> let pli = ni - 1 in
      case lookupEntry pli es of
        Just LogEntry{..} -> (pli, _leTerm)
         -- this shouldn't happen, because nextIndex - 1 should always be at
         -- most our last entry
        Nothing -> (startIndex, startTerm)
    Nothing -> (startIndex, startTerm)


-- | Latest hash or empty
lastLogHash :: Log LogEntry -> ByteString
lastLogHash = maybe mempty _leHash . lastEntry

-- | Latest term on log or 'startTerm'
lastLogTerm :: Log LogEntry -> Term
lastLogTerm = maybe startTerm _leTerm . lastEntry

-- | get entries after index to beginning, with limit, for AppendEntries message.
-- TODO make monadic to get 8000 limit from config.
getEntriesAfter :: LogIndex -> Log a -> Seq a
getEntriesAfter pli = Seq.take 8000 . Seq.drop (fromIntegral $ pli + 1) . _lEntries


-- TODO: This uses the old decode encode trick and should be changed...
hashLogEntry :: Maybe LogEntry -> LogEntry -> LogEntry
hashLogEntry (Just LogEntry{ _leHash = prevHash }) le@LogEntry{..} =
  le { _leHash = hash SHA256 (encode $ LEWire (_leTerm, getCmdSignedRPC le, prevHash))}
hashLogEntry Nothing le@LogEntry{..} =
  le { _leHash = hash SHA256 (encode $ LEWire (_leTerm, getCmdSignedRPC le, mempty))}

getCmdSignedRPC :: LogEntry -> SignedRPC
getCmdSignedRPC LogEntry{ _leCommand = Command{ _cmdProvenance = ReceivedMsg{ _pDig = dig, _pOrig = bdy }}} =
  SignedRPC dig bdy
getCmdSignedRPC LogEntry{ _leCommand = Command{ _cmdProvenance = NewMsg }} =
  error "Invariant Failure: for a command to be in a log entry, it needs to have been received!"

-- | Recursively hash entries from index to tail.
updateLogHashesFromIndex :: LogIndex -> Log LogEntry -> Log LogEntry
updateLogHashesFromIndex i es =
  case lookupEntry i es of
    Just _ -> updateLogHashesFromIndex (succ i) $
              over lEntries (Seq.adjust (hashLogEntry (lookupEntry (i - 1) es)) (fromIntegral i)) es
    Nothing -> es

-- | Append/hash a single entry
appendLogEntry :: LogEntry -> Log LogEntry -> Log LogEntry
appendLogEntry le es =
  case lastEntry es of
    Just ple -> over lEntries (Seq.|> hashLogEntry (Just ple) le) es
    _ -> Log $ Seq.singleton (hashLogEntry Nothing le)

-- | Add/hash entries at specified index.
addLogEntriesAt :: LogIndex -> Seq LogEntry -> Log LogEntry -> Log LogEntry
addLogEntriesAt pli newEs = updateLogHashesFromIndex (pli + 1) .
                            over lEntries ((Seq.>< newEs) . Seq.take (fromIntegral pli + 1))
