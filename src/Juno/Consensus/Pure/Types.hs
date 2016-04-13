{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}

module Juno.Consensus.Pure.Types (
    NodeID(..)
  , CommandEntry(..)
  , CommandResult(..)
  , Term(..), startTerm
  , LogIndex(..), startIndex
  , RequestId(..), startRequestId, toRequestId
  , Role(..)
  , LogEntry(..)
  -- * RPC
  , AppendEntries(..)
  , AppendEntriesResponse(..),AlotOfAERs(..)
  , RequestVote(..)
  , RequestVoteResponse(..)
  , Command(..)
  , CommandResponse(..)
  , CommandBatch(..)
  , Revolution(..)
  , RPC(..)
  , Event(..)
  , MsgType(..), KeySet(..), Digest(..), Provenance(..), WireFormat(..)
  , signedRPCtoRPC, rpcToSignedRPC, verifySignedRPC
  , SignedRPC(..)
  , PrivateKey, PublicKey, Signature(..)
  , ReceivedAt(..)
  ) where


import Juno.Runtime.Types (
    NodeID(..)
  , CommandEntry(..)
  , CommandResult(..)
  , Term(..), startTerm
  , LogIndex(..), startIndex
  , RequestId(..), startRequestId, toRequestId
  , Role(..)
  , LogEntry(..)
  -- * RPC
  , AppendEntries(..)
  , AppendEntriesResponse(..),AlotOfAERs(..)
  , RequestVote(..)
  , RequestVoteResponse(..)
  , Command(..)
  , CommandResponse(..)
  , CommandBatch(..)
  , Revolution(..)
  , RPC(..)
  , Event(..)
  , MsgType(..), KeySet(..), Digest(..), Provenance(..), WireFormat(..)
  , signedRPCtoRPC, rpcToSignedRPC, verifySignedRPC
  , SignedRPC(..)
  , PrivateKey, PublicKey, Signature(..)
  , ReceivedAt(..)
  )
