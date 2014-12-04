module Main
  ( main
  ) where

import Network.Tangaroa.Spec.Simple
import Control.Concurrent.STM

import Command

applyCommand :: TVar Int -> CommandType -> IO ResultType
applyCommand tv cmd = case cmd of
  AddOne      -> atomically $ modifyTVar tv (+ 1) >> return Nothing
  SubtractOne -> atomically $ modifyTVar tv (subtract 1) >> return Nothing
  GetValue    -> fmap Just $ readTVarIO tv

main :: IO ()
main = do
  stateVariable <- newTVarIO 0
  runServer (applyCommand stateVariable)
