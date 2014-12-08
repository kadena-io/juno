{-# LANGUAGE DeriveGeneric #-}

module Command
  ( CommandType(..)
  , ResultType(..)
  ) where

import Data.Binary
import GHC.Generics

data CommandType = Insert String String
                 | Delete String
                 | Set    String String
                 | Get    String
  deriving (Show, Read, Generic)

instance Binary CommandType

data ResultType = Value String -- for successful Get
                | Success      -- for successful Insert, Delete, Set
                | Failure
  deriving (Show, Read, Generic)

instance Binary ResultType
