{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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
  , AppendEntriesResponse(..)
  , RequestVote(..)
  , RequestVoteResponse(..)
  , Command(..)
  , CommandResponse(..)
  , Revolution(..)
  , RPC(..)
  , Event(..)
  , HasSig(..)
  , signRPC
  , verifyRPC
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
  , AppendEntriesResponse(..)
  , RequestVote(..)
  , RequestVoteResponse(..)
  , Command(..)
  , CommandResponse(..)
  , Revolution(..)
  , RPC(..)
  , Event(..)
  , HasSig(..)
  , signRPC
  , verifyRPC
  )
