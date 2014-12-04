module Command
  ( CommandType(..)
  , ResultType(..)
  ) where

data CommandType = Insert String String
                 | Delete String
                 | Set    String String
                 | Get    String
  deriving (Show, Read)

data ResultType = Value String -- for successful Get
                | Success      -- for successful Inserts, Delete, Set
                | Failure
  deriving (Show, Read)
