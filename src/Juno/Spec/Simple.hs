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
import Juno.Runtime.Protocol.Types
import Juno.Messaging.ZMQ
import Juno.Monitoring.Server (startMonitoring)

import Control.Lens
import Control.Monad
import Control.Concurrent (yield, threadDelay, takeMVar, putMVar, newMVar, MVar)
import qualified Control.Concurrent.Lifted as CL
import Control.Concurrent.Chan.Unagi
import qualified Control.Concurrent.Chan.Unagi.NoBlocking as NoBlock
import qualified Control.Concurrent.Chan.Unagi.Bounded as Bounded

import Data.Thyme.Clock (getCurrentTime)

import System.Console.GetOpt
import System.Environment
import System.Exit
import Data.Thyme.LocalTime


import Data.ByteString (ByteString)

import Control.Monad.IO.Class

import qualified Data.Yaml as Y

import System.Random

data Options = Options
  { optConfigFile :: FilePath
  } deriving Show

defaultOptions :: Options
defaultOptions = Options { optConfigFile = "" }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['c']
           ["config"]
           (ReqArg (\fp opts -> opts { optConfigFile = fp }) "CONF_FILE")
           "Configuration File"
  ]

getConfig :: IO Config
getConfig = do
  argv <- getArgs
  case getOpt Permute options argv of
    (o,_,[]) -> do
      opts <- return $ foldl (flip id) defaultOptions o
      conf <- Y.decodeFileEither $ optConfigFile opts
      case conf of
        Left err -> putStrLn (Y.prettyPrintParseException err) >> exitFailure
        Right conf' -> return conf'
    (_,_,errs)     -> mapM_ putStrLn errs >> exitFailure

showDebug :: NodeID -> String -> IO ()
showDebug _ msg = do
  (ZonedTime (LocalTime _ t) _) <- getZonedTime
  putStrLn $ (take 15 $ show t) ++ " " ++ msg

noDebug :: NodeID -> String -> IO ()
noDebug _ _ = return ()

simpleRaftSpec :: MonadIO m
               => MVar (NoBlock.Stream (ReceivedAt, SignedRPC))
               -> MVar (NoBlock.Stream (ReceivedAt, SignedRPC))
               -> MVar (NoBlock.Stream (ReceivedAt, SignedRPC))
               -> InChan (OutBoundMsg String ByteString)
               -> Bounded.OutChan Event
               -> Bounded.InChan Event
               -> (CommandEntry -> m CommandResult)
               -> (NodeID -> String -> m ())
               -> (Metric -> m ())
               -> RaftSpec m
simpleRaftSpec inboxRead cmdInboxRead aerInboxRead outboxWrite eventRead eventWrite applyFn debugFn pubMetricFn = RaftSpec
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
    , _sendMessages    = liftIO . sendMsgs outboxWrite
      -- get messages using getMsg
    , _getMessage      = liftIO $ getMsgSync inboxRead
    , _getMessages     = liftIO . getBacklog inboxRead
    , _getNewCommands  = liftIO . getBacklog cmdInboxRead
    , _getNewEvidence  = liftIO . getBacklog aerInboxRead
      -- use the debug function given by the caller
    , _debugPrint      = debugFn
      -- publish a 'Metric' to EKG
    , _publishMetric   = pubMetricFn
      -- get the current time in UTC
    , _getTimestamp = liftIO getCurrentTime
     -- _random :: forall a . Random a => (a, a) -> m a
    , _random = liftIO . randomRIO
    -- _enqueue :: InChan (Event nt et rt) -> Event nt et rt -> m ()
    , _enqueue = \e -> liftIO $ Bounded.writeChan eventWrite e >> yield

    , _enqueueMultiple = \e -> liftIO $ Bounded.writeList2Chan eventWrite e >> yield

    -- _enqueueLater :: Int -> InChan (Event nt et rt) -> Event nt et rt -> m ThreadId
    , _enqueueLater = \t e -> liftIO $ CL.fork (CL.threadDelay t >> liftIO (Bounded.writeChan eventWrite e))

    , _killEnqueued = liftIO . CL.killThread

    -- _dequeue :: OutChan (Event nt et rt) -> m (Event nt et rt)
    , _dequeue = liftIO $ Bounded.readChan eventRead

    }

getMsgSync :: MVar (NoBlock.Stream (ReceivedAt,SignedRPC)) -> IO (ReceivedAt,SignedRPC)
getMsgSync m = do
  inboxRead <- takeMVar m
  t <- NoBlock.tryReadNext inboxRead
  case t of
    NoBlock.Pending -> putMVar m inboxRead >> getMsgSync m
    NoBlock.Next v inboxRead' -> putMVar m inboxRead' >> return v

getBacklog :: MVar (NoBlock.Stream (ReceivedAt,SignedRPC)) -> Int -> IO [(ReceivedAt,SignedRPC)]
getBacklog m cnt = do
  inboxRead <- takeMVar m
  let go strm cnt' = if cnt' <= 0
                then putMVar m strm >> return []
                else do
                  s <- NoBlock.tryReadNext strm
                  case s of
                    NoBlock.Next a strm' -> liftM (a:) (go strm' (cnt'-1))
                    NoBlock.Pending -> putMVar m strm >> return []
  blog <- go inboxRead cnt
  if blog /= []
    then return blog
    else threadDelay 1000 >> return []

nodeIDtoAddr :: NodeID -> Addr String
nodeIDtoAddr (NodeID _ p) = Addr $ "tcp://127.0.0.1:" ++ show p

toMsg :: NodeID -> msg -> OutBoundMsg String msg
toMsg n b = OutBoundMsg (ROne $ nodeIDtoAddr n) b

sendMsgs :: InChan (OutBoundMsg String ByteString) -> [(NodeID, ByteString)] -> IO ()
sendMsgs outboxWrite ns = do
  writeList2Chan outboxWrite $! uncurry toMsg <$> ns
  yield

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
  (inboxWrite, inboxRead) <- NoBlock.newChan
  inboxRead' <- newMVar =<< return . head =<< NoBlock.streamChan 1 inboxRead
  (cmdInboxWrite, cmdInboxRead) <- NoBlock.newChan
  cmdInboxRead' <- newMVar =<< return . head =<< NoBlock.streamChan 1 cmdInboxRead
  (aerInboxWrite, aerInboxRead) <- NoBlock.newChan
  aerInboxRead' <- newMVar =<< return . head =<< NoBlock.streamChan 1 aerInboxRead
  (outboxWrite, outboxRead) <- newChan
  -- the 50 provides the back pressure on the msg stream.
  -- When the writer blocks, the messages will build up in the inboxRead, which is unbounded
  (eventWrite, eventRead) <- Bounded.newChan 20
  let debugFn = if (rconf ^. enableDebug) then showDebug else noDebug
  pubMetric <- startMonitoring rconf
  runMsgServer inboxWrite cmdInboxWrite aerInboxWrite outboxRead me []
  let raftSpec = simpleRaftSpec inboxRead' cmdInboxRead' aerInboxRead' outboxWrite eventRead eventWrite (liftIO . applyFn) (liftIO2 debugFn) (liftIO . pubMetric)
  runRaftServer rconf raftSpec

runClient :: (CommandEntry -> IO CommandResult) -> IO (RequestId, [CommandEntry]) -> CommandMVarMap -> IO ()
runClient applyFn getEntries cmdStatusMap = do
  rconf <- getConfig
  me <- return $ nodeIDtoAddr $ rconf ^. nodeId
  (inboxWrite, inboxRead) <- NoBlock.newChan
  inboxRead' <- newMVar =<< return . head =<< NoBlock.streamChan 1 inboxRead
  (cmdInboxWrite, cmdInboxRead) <- NoBlock.newChan
  cmdInboxRead' <- newMVar =<< return . head =<< NoBlock.streamChan 1 cmdInboxRead
  (aerInboxWrite, aerInboxRead) <- NoBlock.newChan
  aerInboxRead' <- newMVar =<< return . head =<< NoBlock.streamChan 1 aerInboxRead
  (outboxWrite, outboxRead) <- newChan -- raft writes to outbox, client reads
  (eventWrite, eventRead) <- Bounded.newChan 20 -- timer events
  let debugFn = if (rconf ^. enableDebug) then showDebug else noDebug
  pubMetric <- startMonitoring rconf
  runMsgServer inboxWrite cmdInboxWrite aerInboxWrite outboxRead me [] -- ZMQ
  let raftSpec = simpleRaftSpec inboxRead' cmdInboxRead' aerInboxRead' outboxWrite eventRead eventWrite (liftIO . applyFn) (liftIO2 debugFn) (liftIO . pubMetric)
  runRaftClient getEntries cmdStatusMap rconf raftSpec


-- | lift a two-arg action into MonadIO
liftIO2 :: MonadIO m => (a -> b -> IO c) -> a -> b -> m c
liftIO2 f a = liftIO . f a
