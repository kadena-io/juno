module Main
  ( main
  ) where

import Network.Tangaroa

import Control.Concurrent

import Text.Read

import qualified Data.Set as Set

-- trivial raft spec where everything is a String and handles are ()
raftspec :: RaftSpec String String String String ()
raftspec = RaftSpec
  { -- constant configuration
    __readCfg         = return dummyConfig
    -- all log entries are ""
  , __readLogEntry    = return . const ""
    -- don't write log entries
  , __writeLogEntry   = \_ _ -> return ()
    -- always read startTerm
  , __readTermNumber  = return startTerm
    -- don't write term numbers
  , __writeTermNumber = return . const ()
    -- never voted for anyone
  , __readVotedFor    = return Nothing
    -- don't record votes
  , __writeVotedFor   = return . const ()
    -- apply a log entry by showing to stdout and returning empty string
  , __applyLogEntry  = \e -> putStrLn e >> return ""
    -- don't open a connection
  , __openConnection  = return . const ()
    -- serialize with show
  , __serializeRPC    = show
    -- deserialize with readMaybe
  , __deserializeRPC  = readMaybe
    -- don't send messages
  , __sendMessage     = \_ _ -> return ()
    -- get dummy messages every 5 seconds
  , __getMessage      = \_ -> threadDelay 5000000 >> (return $ show dummyMessage)
    -- use putStrLn for debug messages
  , __debugPrint      = putStrLn
  }

dummyMessage :: RPC String String String
dummyMessage = DBG "A message!"

dummyConfig :: Config String
dummyConfig = Config (Set.fromList ["node0"]) "node0" (5000000,10000000) 1000000

main :: IO ()
main = runRaft raftspec
