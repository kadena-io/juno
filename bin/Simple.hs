{-# LANGUAGE DeriveGeneric #-}

module Main
  ( main
  ) where

import Network.Tangaroa

import Control.Concurrent.STM
import Control.Lens
import Data.Word
import System.Console.GetOpt
import System.Environment
import System.Exit
import GHC.Generics
import Network.Socket

import Text.Read

import qualified Data.Set as Set

type NodeType = (HostAddress, Word16)
data EntryType = AddOne | SubtractOne | GetValue
  deriving (Show,Read,Generic)
type ResultType = Maybe Int

localhost :: HostAddress
localhost = 0x0100007f

defaultPortNum :: Word16
defaultPortNum = 10000

defaultConfig :: Config NodeType
defaultConfig =
  Config
    Set.empty          -- other nodes
    (localhost,defaultPortNum)  -- self address
    (6000000,12000000) -- election timeout range
    3000000            -- heartbeat timeout

options :: [OptDescr (Config NodeType -> Config NodeType)]
options =
  [ Option ['s'] ["self"]
    (ReqArg setThisNode "SELF_PORT_NUMBER")
    "The port number of this node."
  ]

applyEntry :: TVar Int -> EntryType -> IO ResultType
applyEntry tv cmd = case cmd of
  AddOne      -> atomically $ modifyTVar tv (+ 1) >> return Nothing
  SubtractOne -> atomically $ modifyTVar tv (subtract 1) >> return Nothing
  GetValue    -> fmap Just $ readTVarIO tv

nodeSockAddr :: NodeType -> SockAddr
nodeSockAddr (host,port) = SockAddrInet (PortNum port) host

setThisNode :: String -> Config NodeType -> Config NodeType
setThisNode =
  maybe id (\p -> nodeId .~ (localhost, p)) . readMaybe

addOtherNode :: String -> Config NodeType -> Config NodeType
addOtherNode =
  maybe id (\p -> otherNodes %~ Set.insert (localhost, p)) .  readMaybe

getConfig :: IO (Config NodeType)
getConfig = do
  argv <- getArgs
  case getOpt Permute options argv of
    (opts,args,[]) -> return $ foldr addOtherNode (foldr ($) defaultConfig opts) args
    (_,_,_)        -> exitFailure -- TODO, print errors

getMsg :: Socket -> IO String
getMsg sock = recv sock 8192

msgSend :: Socket -> NodeType -> String -> IO ()
msgSend sock node s =
  sendTo sock s (nodeSockAddr node) >> return ()

main :: IO ()
main = do
  rconf <- getConfig
  sock <- socket AF_INET Datagram defaultProtocol
  bind sock $ nodeSockAddr $ rconf ^. nodeId
  stateVariable <- newTVarIO (0 :: Int)
  runRaft rconf $ RaftSpec
    {
      -- TODO all log entries are GetValue
      __readLogEntry    = return . const GetValue
      -- TODO don't write log entries
    , __writeLogEntry   = \_ _ -> return ()
      -- TODO always read startTerm
    , __readTermNumber  = return startTerm
      -- TODO don't write term numbers
    , __writeTermNumber = return . const ()
      -- TODO never voted for anyone
    , __readVotedFor    = return Nothing
      -- TODO don't record votes
    , __writeVotedFor   = return . const ()
      -- apply a log entry by updating the state variable
    , __applyLogEntry   = applyEntry stateVariable
      -- serialize with show
    , __serializeRPC    = show
      -- deserialize with readMaybe
    , __deserializeRPC  = readMaybe
      -- send messages using msgSend
    , __sendMessage     = msgSend sock
      -- get messages using getMsg
    , __getMessage      = getMsg sock
      -- use putStrLn for debug messages
    , __debugPrint      = putStrLn
    }
