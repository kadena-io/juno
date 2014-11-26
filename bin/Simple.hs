module Main
  ( main
  ) where

import Network.Tangaroa

import Control.Concurrent

import qualified Data.Set as Set

-- trivial raft spec where nodes and log entries are strings
-- command results are (), and handles are ()
-- and the serialization type is also String
raftspec :: RaftSpec String String () String ()
raftspec = RaftSpec
  { -- commit by showing to stdout
    commit          = putStrLn
    -- constant persistent state
  , getPS           = return (PersistentState startTerm Nothing ["foo"])
    -- constant configuration
  , getCfg          = return (Config (Set.fromList ["node0"]) "node0" (5000000,10000000) 1000000)
    -- don't write persistent state
  , writePS         = return . const ()
    -- don't open a connection
  , openConnection  = return . const ()
    -- serialize with show
  , serializeResult = show
    -- serialize with show
  , serializeRPC    = show
    -- deserialize with read
  , deserializeRPC  = read
    -- don't send messages
  , sendMessage     = \_ _ -> return ()
    -- get dummy messages every 5 seconds
  , getMessage      = \_ -> do threadDelay 5000000; return "A message!"
  }

main :: IO ()
main = runRaft raftspec
