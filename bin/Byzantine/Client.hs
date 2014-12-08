module Main
  ( main
  ) where

import Network.Tangaroa.Byzantine.Spec.Simple

import Command

getCommand :: IO CommandType
getCommand = do
  cmd <- getLine
  case words cmd of
    ["insert", k, v] -> return (Insert k v)
    ["delete", k]    -> return (Delete k)
    ["set", k, v]    -> return (Set k v)
    ["get", k]       -> return (Get k)
    _        -> do
      putStrLn "Not a recognized command."
      getCommand

showResult :: ResultType -> IO ()
showResult r =
  case r of
    Value v -> putStrLn v
    _       -> print r

main :: IO ()
main = do
  runClient (\_ -> return Failure) getCommand showResult
