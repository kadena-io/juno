module Main
  ( main
  ) where

import Network.Tangaroa.Byzantine.Spec.Simple

import Data.IORef
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map

import Command

applyCommand :: IORef (Map String String) -> CommandType -> IO ResultType
applyCommand ref cmd = do
  case cmd of
    Insert k v -> runInsert ref k v
    Delete k   -> runDelete ref k
    Set    k v -> runSet    ref k v
    Get    k   -> runGet    ref k

member :: IORef (Map String String)
       -> String
       -> (IORef (Map String String) -> IO ResultType)
       -> (IORef (Map String String) -> IO ResultType)
       -> IO ResultType
member ref k memberFn notMemberFn = do
  isMember <- Map.member k <$> readIORef ref
  if isMember
    then memberFn ref
    else notMemberFn ref

-- adds a new mapping, and fails if a mapping already exists
runInsert :: IORef (Map String String) -> String -> String -> IO ResultType
runInsert ref k v = member ref k doFail (doInsert k v)

-- like insert, but instead fails if a mapping doesn't exist
runSet :: IORef (Map String String) -> String -> String -> IO ResultType
runSet ref k v = member ref k (doInsert k v) doFail

-- gets the value for a key, fails if it doesn't exist
runGet :: IORef (Map String String) -> String -> IO ResultType
runGet ref k = do
  mv <- Map.lookup k <$> readIORef ref
  case mv of
    Just v  -> return (Value v)
    Nothing -> return Failure

-- removes the mapping for a key, fails if it doesn't exist
runDelete :: IORef (Map String String) -> String -> IO ResultType
runDelete ref k = member ref k (doDelete k) doFail

doFail :: IORef (Map String String) -> IO ResultType
doFail = return . const Failure

doInsert :: String -> String -> IORef (Map String String) -> IO ResultType
doInsert k v ref = modifyIORef ref (Map.insert k v) >> return Success

doDelete :: String -> IORef (Map String String) -> IO ResultType
doDelete k ref = modifyIORef ref (Map.delete k) >> return Success

main :: IO ()
main = do
  stateVariable <- newIORef Map.empty
  runServer (applyCommand stateVariable)
