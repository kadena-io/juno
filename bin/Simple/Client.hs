module Main
  ( main
  ) where

import Network.Tangaroa.Spec.Simple
import System.Exit

import Command

getCommand :: IO CommandType
getCommand = do
  cmd <- getLine
  case words cmd of
    ["add"]  -> return AddOne
    ["sub"]  -> return SubtractOne
    ["get"]  -> return GetValue
    ["exit"] -> exitSuccess
    _        -> do
      putStrLn "Not a recognized command."
      getCommand

showResult :: ResultType -> IO ()
showResult r =
  case r of
    Just v  -> print v
    Nothing -> putStrLn "success"

main :: IO ()
main = do
  runClient (\_ -> return Nothing) getCommand showResult
