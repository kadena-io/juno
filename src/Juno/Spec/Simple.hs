{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Juno.Spec.Simple
  ( runServer
  , runClient
  , RequestId
  , CommandStatus
  ) where

import Juno.Consensus.ByzRaft.Server
import Juno.Consensus.ByzRaft.Client
import Juno.Runtime.Types
import Juno.Messaging.Types
import Juno.Messaging.ZMQ

import Control.Lens
import Control.Concurrent (yield)
import qualified Control.Concurrent.Lifted as CL
import Control.Concurrent.Chan.Unagi
import Data.Word

import System.Console.GetOpt
import System.Environment
import System.Exit
import Data.Thyme.LocalTime
import Text.Read
import qualified Data.Set as Set
import Data.ByteString (ByteString)

import Control.Monad.IO.Class
import qualified Data.Map as Map

import System.Random


readNodeID :: String -> Either String NodeID
readNodeID s = case break (==':') s of
                   (h@(_:_),(_:p)) -> case readMaybe p of
                                        Just v -> return $ NodeID h v
                                        Nothing -> Left $ "Invalid port: " ++ s
                   _ -> Left $ "Invalid host:port value: " ++ s

options :: [OptDescr (Config -> IO Config)]
options =
  [ Option ['s'] ["self"]
    (ReqArg setSelf "SELF_NODE")
    "This node as HOST:PORT"
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

getConfig :: IO Config
getConfig = do
  argv <- getArgs
  case getOpt Permute options argv of
    (opts,args,[]) -> cfgFold (opts ++ map addOtherNode args) defaultConfig
    (_,_,errs)     -> mapM_ putStrLn errs >> exitFailure

localhost :: String
localhost = "127.0.0.1"

defaultPortNum :: Word64
defaultPortNum = 10000

defaultConfig :: Config
defaultConfig =
  Config
    Set.empty                                  -- other nodes
    (NodeID localhost defaultPortNum)          -- self address
    Map.empty                                  -- publicKeys
    Map.empty                                  -- clientPublicKeys
    (SecretKey "") -- empty public key
    (PublicKey "") -- empty public key
    (3000000,6000000)                          -- election timeout range
    1500000                                    -- heartbeat timeout
    False                                      -- no debug
    5000000                                     -- client timeouts before revolution

setSelf :: String -> Config -> IO Config
setSelf s c = case readNodeID s of
                  Left e -> die e
                  Right p -> return $ set nodeId p c

addOtherNode :: String -> Config -> IO Config
addOtherNode s c = case readNodeID s of
                     Left e -> die e
                     Right p -> return $ over otherNodes (Set.insert p) c

readFileOrDie :: Read a => Lens' s a -> FilePath -> s -> IO s
readFileOrDie l filename conf = do
  contents <- readFile filename
  keys <- return $ readMaybe contents
  case keys of
    Just pkm -> return $ conf & l .~ pkm
    Nothing  -> die $ "file invalid (" ++ filename ++ ")"

getPublicKeys :: FilePath -> Config -> IO Config
getPublicKeys = readFileOrDie publicKeys

getClientPublicKeys :: FilePath -> Config -> IO Config
getClientPublicKeys = readFileOrDie clientPublicKeys

getPrivateKey :: FilePath -> Config -> IO Config
getPrivateKey fp c = do
  c' <- readFileOrDie myPrivateKey fp c
  return $ c' { _myPublicKey = toPublicKey $ c' ^. myPrivateKey }

showDebug :: NodeID -> String -> IO ()
showDebug _ msg = do
  (ZonedTime (LocalTime _ t) _) <- getZonedTime
  putStrLn $ (take 11 $ show t) ++ " " ++ msg

noDebug :: NodeID -> String -> IO ()
noDebug _ _ = return ()

simpleRaftSpec :: MonadIO m
               => OutChan ByteString
               -> InChan (OutBoundMsg String ByteString)
               -> OutChan Event
               -> InChan Event
               -> (CommandEntry -> m CommandResult)
               -> (NodeID -> String -> m ())
               -> RaftSpec m
simpleRaftSpec inboxRead outboxWrite eventRead eventWrite applyFn debugFn = RaftSpec
    {
      -- TODO don't read log entries
      _readLogEntry    = return . const Nothing
      -- TODO don't write log entries
    , _writeLogEntry   = \_ _ -> return ()
      -- TODO always read startTerm
    , _readTermNumber  = return startTerm
      -- TODO don't write term numbers
    , _writeTermNumber = return . const ()
      -- TODO never voted for anyone
    , _readVotedFor    = return Nothing
      -- TODO don't record votes
    , _writeVotedFor   = return . const ()
      -- apply log entries to the state machine, given by caller
    , _applyLogEntry   = applyFn
      -- send messages using msgSend
    , _sendMessage     = liftIO2 (sendMsg outboxWrite)
      -- get messages using getMsg
    , _getMessage      = liftIO $ readChan inboxRead
      -- use the debug function given by the caller
    , _debugPrint      = debugFn

    -- _random :: forall a . Random a => (a, a) -> m a
    , _random = liftIO . randomRIO
    -- _enqueue :: InChan (Event nt et rt) -> Event nt et rt -> m ()
    , _enqueue = \e -> liftIO $ writeChan eventWrite e >> yield

    -- _enqueueLater :: Int -> InChan (Event nt et rt) -> Event nt et rt -> m ThreadId
    , _enqueueLater = \t e -> liftIO $ CL.fork (CL.threadDelay t >> liftIO (writeChan eventWrite e))

    , _killEnqueued = liftIO . CL.killThread

    -- _dequeue :: OutChan (Event nt et rt) -> m (Event nt et rt)
    , _dequeue = liftIO $ readChan eventRead

    }

nodeIDtoAddr :: NodeID -> Addr String
nodeIDtoAddr (NodeID _ p) = Addr $ "tcp://127.0.0.1:" ++ show p

sendMsg :: InChan (OutBoundMsg String ByteString) -> NodeID -> ByteString -> IO ()
sendMsg outboxWrite n s = do
  let addr = ROne $ nodeIDtoAddr n
      msg = OutBoundMsg addr s
  writeChan outboxWrite msg
  yield

runServer :: (CommandEntry -> IO CommandResult) -> IO ()
runServer applyFn = do
  rconf <- getConfig
  me <- return $ nodeIDtoAddr $ rconf ^. nodeId
  (inboxWrite, inboxRead) <- newChan
  (outboxWrite, outboxRead) <- newChan
  (eventWrite, eventRead) <- newChan
  let debugFn = if (rconf ^. enableDebug) then showDebug else noDebug
  runMsgServer inboxWrite outboxRead me []
  runRaftServer rconf $ simpleRaftSpec inboxRead outboxWrite eventRead eventWrite (liftIO . applyFn) (liftIO2 debugFn)

runClient :: (CommandEntry -> IO CommandResult) -> IO (RequestId, CommandEntry) -> CommandMVarMap -> IO ()
runClient applyFn getEntry cmdStatusMap = do
  rconf <- getConfig
  me <- return $ nodeIDtoAddr $ rconf ^. nodeId
  (inboxWrite, inboxRead) <- newChan -- client writes to inbox, raft reads
  (outboxWrite, outboxRead) <- newChan -- raft writes to outbox, client reads
  (eventWrite, eventRead) <- newChan -- timer events
  let debugFn = if (rconf ^. enableDebug) then showDebug else noDebug
  runMsgServer inboxWrite outboxRead me [] -- ZMQ
  runRaftClient getEntry cmdStatusMap rconf (simpleRaftSpec inboxRead outboxWrite eventRead eventWrite (liftIO . applyFn) (liftIO2 debugFn))


-- | lift a two-arg action into MonadIO
liftIO2 :: MonadIO m => (a -> b -> IO c) -> a -> b -> m c
liftIO2 f a = liftIO . f a
