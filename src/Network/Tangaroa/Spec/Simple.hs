module Network.Tangaroa.Spec.Simple
  ( runServer
  , runClient
  ) where

import Network.Tangaroa
import Network.Tangaroa.Client

import Control.Lens
import Data.Word
import Network.Socket
import System.Console.GetOpt
import System.Environment
import System.Exit
import Text.Read
import qualified Data.Set as Set

options :: [OptDescr (Config NodeType -> Config NodeType)]
options =
  [ Option ['s'] ["self"]
    (ReqArg setThisNode "SELF_PORT_NUMBER")
    "The port number of this node."
  , Option ['d'] ["debug"]
    (NoArg (enableDebug .~ True))
    "Enable debugging info (show RPCs and timeouts)."
  ]

getConfig :: IO (Config NodeType)
getConfig = do
  argv <- getArgs
  case getOpt Permute options argv of
    (opts,args,[]) -> return $ foldr addOtherNode (foldr ($) defaultConfig opts) args
    (_,_,_)        -> exitFailure -- TODO, print errors

type NodeType = (HostAddress, Word16)
localhost :: HostAddress
localhost = 0x0100007f

defaultPortNum :: Word16
defaultPortNum = 10000

defaultConfig :: Config NodeType
defaultConfig =
  Config
    Set.empty                  -- other nodes
    (localhost,defaultPortNum) -- self address
    (3000000,6000000)          -- election timeout range
    1500000                    -- heartbeat timeout
    False                      -- no debug

nodeSockAddr :: NodeType -> SockAddr
nodeSockAddr (host,port) = SockAddrInet (PortNum port) host

setThisNode :: String -> Config NodeType -> Config NodeType
setThisNode =
  maybe id (\p -> nodeId .~ (localhost, p)) . readMaybe

addOtherNode :: String -> Config NodeType -> Config NodeType
addOtherNode =
  maybe id (\p -> otherNodes %~ Set.insert (localhost, p)) . readMaybe

getMsg :: Socket -> IO String
getMsg sock = recv sock 8192

msgSend :: Socket -> NodeType -> String -> IO ()
msgSend sock node s =
  sendTo sock s (nodeSockAddr node) >> return ()

showDebug :: NodeType -> String -> IO ()
showDebug node msg = putStrLn $ show (snd node) ++ " " ++ msg

noDebug :: NodeType -> String -> IO ()
noDebug _ _ = return ()

simpleRaftSpec :: (Show et, Read et, Show rt, Read rt)
               => Socket
               -> (et -> IO rt)
               -> (NodeType -> String -> IO ())
               -> RaftSpec NodeType et rt String
simpleRaftSpec sock applyFn debugFn = RaftSpec
    {
      -- TODO don't read log entries
      __readLogEntry    = return . const Nothing
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
      -- apply log entries to the state machine, given by caller
    , __applyLogEntry   = applyFn
      -- serialize with show
    , __serializeRPC    = show
      -- deserialize with readMaybe
    , __deserializeRPC  = readMaybe
      -- send messages using msgSend
    , __sendMessage     = msgSend sock
      -- get messages using getMsg
    , __getMessage      = getMsg sock
      -- use the debug function given by the caller
    , __debugPrint      = debugFn
    }

runServer :: (Show et, Read et, Show rt, Read rt) =>
  (et -> IO rt) -> IO ()
runServer applyFn = do
  rconf <- getConfig
  sock <- socket AF_INET Datagram defaultProtocol
  bind sock $ nodeSockAddr $ rconf ^. nodeId
  let debugFn = if (rconf ^. enableDebug) then showDebug else noDebug
  runRaft rconf $ simpleRaftSpec sock applyFn debugFn

runClient :: (Show et, Read et, Show rt, Read rt) =>
  (et -> IO rt) -> IO et -> (rt -> IO ()) -> IO ()
runClient applyFn getEntry useResult = do
  rconf <- getConfig
  sock <- socket AF_INET Datagram defaultProtocol
  bind sock $ nodeSockAddr $ rconf ^. nodeId
  let debugFn = if (rconf ^. enableDebug) then showDebug else noDebug
  runRaftClient getEntry useResult rconf (simpleRaftSpec sock applyFn debugFn)
