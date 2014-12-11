module Network.Tangaroa.Byzantine.Spec.Simple
  ( runServer
  , runClient
  ) where

import Network.Tangaroa.Byzantine.Server
import Network.Tangaroa.Byzantine.Client
import Network.Tangaroa.Byzantine.Types

import Control.Lens
import Data.Word
import Data.Binary
import Network.Socket
import System.Console.GetOpt
import System.Environment
import System.Exit
import Text.Read
import qualified Data.Set as Set
import qualified Data.Map as Map
import Codec.Crypto.RSA

options :: [OptDescr (Config NodeType -> IO (Config NodeType))]
options =
  [ Option ['s'] ["self"]
    (ReqArg setThisNode "SELF_PORT_NUMBER")
    "The port number of this node."
  , Option ['d'] ["debug"]
    (NoArg (return . (enableDebug .~ True)))
    "Enable debugging info (show RPCs and timeouts)."
  , Option ['p'] ["public-keys"]
    (ReqArg getPublicKeys "NODE_PUBLIC_KEY_FILE")
    "A file containing a map of nodes to their public key."
  , Option ['c'] ["client-keys"]
    (ReqArg getClientPublicKeys "CLIENT_PUBLIC_KEY_FILE")
    "A file containing a map of clients to their public key."
  , Option ['k'] ["private-key"]
    (ReqArg getPrivateKey "PRIVATE_KEY_FILE")
    "A file containing the node's private key."
  ]

cfgFold :: [a -> IO a] -> a -> IO a
cfgFold [] x = return x
cfgFold (f:fs) x = do
  fx <- f x
  cfgFold fs fx

getConfig :: IO (Config NodeType)
getConfig = do
  argv <- getArgs
  case getOpt Permute options argv of
    (opts,args,[]) -> cfgFold (opts ++ map addOtherNode args) defaultConfig
    (_,_,errs)     -> mapM_ putStrLn errs >> exitFailure

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
    Map.empty                  -- publicKeys
    Map.empty                  -- clientPublicKeys
    (PrivateKey (PublicKey 0 0 0) 0 0 0 0 0 0) -- empty public key
    (3000000,6000000)          -- election timeout range
    1500000                    -- heartbeat timeout
    False                      -- no debug

nodeSockAddr :: NodeType -> SockAddr
nodeSockAddr (host,port) = SockAddrInet (PortNum port) host

setThisNode :: String -> Config NodeType -> IO (Config NodeType)
setThisNode s =
  return . maybe id (\p -> nodeId .~ (localhost, p)) (readMaybe s)

addOtherNode :: String -> Config NodeType -> IO (Config NodeType)
addOtherNode s =
  return . maybe id (\p -> otherNodes %~ Set.insert (localhost, p)) (readMaybe s)

getPublicKeys :: FilePath -> Config NodeType -> IO (Config NodeType)
getPublicKeys filename conf = do
  contents <- readFile filename
  return $ case readMaybe contents of
    Just pkm -> conf & publicKeys .~ pkm
    Nothing  -> conf

getClientPublicKeys :: FilePath -> Config NodeType -> IO (Config NodeType)
getClientPublicKeys filename conf = do
  contents <- readFile filename
  return $ case readMaybe contents of
    Just pkm -> conf & clientPublicKeys .~ pkm
    Nothing  -> conf

getPrivateKey :: FilePath -> Config NodeType -> IO (Config NodeType)
getPrivateKey filename conf = do
  contents <- readFile filename
  return $ case readMaybe contents of
    Just pk -> conf & privateKey .~ pk
    Nothing -> conf

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

runServer :: (Binary et, Binary rt, Show et, Read et, Show rt, Read rt) =>
  (et -> IO rt) -> IO ()
runServer applyFn = do
  rconf <- getConfig
  sock <- socket AF_INET Datagram defaultProtocol
  bind sock $ nodeSockAddr $ rconf ^. nodeId
  let debugFn = if (rconf ^. enableDebug) then showDebug else noDebug
  runRaftServer rconf $ simpleRaftSpec sock applyFn debugFn

runClient :: (Binary et, Binary rt, Show et, Read et, Show rt, Read rt) =>
  (et -> IO rt) -> IO et -> (rt -> IO ()) -> IO ()
runClient applyFn getEntry useResult = do
  rconf <- getConfig
  sock <- socket AF_INET Datagram defaultProtocol
  bind sock $ nodeSockAddr $ rconf ^. nodeId
  let debugFn = if (rconf ^. enableDebug) then showDebug else noDebug
  runRaftClient getEntry useResult rconf (simpleRaftSpec sock applyFn debugFn)
