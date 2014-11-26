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
  {
    -- constant configuration
    readCfg          = return (Config (Set.fromList ["node0"]) "node0" (5000000,10000000) 1000000)
    -- all log entries are ""
  , readLogEntry    = return . const ""
    -- don't write log entries
  , writeLogEntry    = \_ _ -> return ()
    -- always read startTerm
  , readTermNumber = return startTerm
    -- don't write term numbers
  , writeTermNumber = return . const ()
    -- never voted for anyone
  , readVotedFor    = return Nothing
    -- don't record votes
  , writeVotedFor   = return . const ()
    -- commit by showing to stdout
  , commit          = putStrLn
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
