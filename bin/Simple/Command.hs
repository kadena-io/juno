module Command
  ( CommandType(..)
  , ResultType
  ) where

data CommandType = AddOne | SubtractOne | GetValue
  deriving (Show, Read)

type ResultType = Maybe Int
