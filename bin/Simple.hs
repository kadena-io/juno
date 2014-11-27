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
  {
    -- constant configuration
    _readCfg         = return dummyConfig
    -- all log entries are ""
  , _readLogEntry    = return . const ""
    -- don't write log entries
  , _writeLogEntry   = \_ _ -> return ()
    -- always read startTerm
  , _readTermNumber  = return startTerm
    -- don't write term numbers
  , _writeTermNumber = return . const ()
    -- never voted for anyone
  , _readVotedFor    = return Nothing
    -- don't record votes
  , _writeVotedFor   = return . const ()
    -- commit by showing to stdout and returning empty string
  , _commit          = \e -> putStrLn e >> return ""
    -- don't open a connection
  , _openConnection  = return . const ()
    -- serialize with show
  , _serializeRPC    = show
    -- deserialize with readMaybe
  , _deserializeRPC  = readMaybe
    -- don't send messages
  , _sendMessage     = \_ _ -> return ()
    -- get dummy messages every 5 seconds
  , _getMessage      = \_ -> threadDelay 5000000 >> (return $ show dummyMessage)
  }

dummyMessage :: RPC String String String
dummyMessage = DBG "A message!"

dummyConfig :: Config String
dummyConfig = Config (Set.fromList ["node0"]) "node0" (5000000,10000000) 1000000

main :: IO ()
main = runRaft raftspec
