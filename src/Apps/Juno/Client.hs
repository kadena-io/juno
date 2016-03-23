{-# LANGUAGE OverloadedStrings #-}

module Apps.Juno.Client
  ( main
  ) where

import Control.Concurrent.Chan.Unagi.Bounded
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Lifted (threadDelay)

import Data.Either ()
import Data.Maybe (catMaybes)
import System.IO

import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BSC
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import Snap.Http.Server
import Snap.Core
import Control.Lens hiding ((.=))
import Data.Ratio
import Data.Aeson (encode, object, (.=))
import qualified Data.Aeson as JSON

import Snap.CORS
import Data.List
import Data.Monoid
import qualified Data.Map as Map
import           Control.Concurrent.MVar
import qualified Control.Concurrent.Lifted as CL
import Control.Monad

import Schwifty.Swift.M105.Types
import Schwifty.Swift.M105.Parser

import Juno.Spec.Simple
import Juno.Runtime.Types (CommandEntry(..), CommandResult(..),CommandStatus(..))
import Juno.Consensus.ByzRaft.Client (CommandMVarMap, CommandMap(..), initCommandMap, setNextCmdRequestId)

import Apps.Juno.JsonTypes
import Apps.Juno.Parser
import Apps.Juno.Ledger

prompt :: String
prompt = "\ESC[0;31mhopper>> \ESC[0m"

promptGreen :: String
promptGreen = "\ESC[0;32mresult>> \ESC[0m"

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: IO String
readPrompt = flushStr prompt >> getLine

-- should we poll here till we get a result?
showResult :: CommandMVarMap -> RequestId -> IO ()
showResult cmdStatusMap rId =
  threadDelay 1000 >> do
    (CommandMap _ m) <- readMVar cmdStatusMap
    case Map.lookup rId m of
      Nothing -> putStrLn "Sorry, something went wrong"
      Just (CmdApplied (CommandResult x)) -> putStrLn $ promptGreen ++ BSC.unpack x
      Just _ -> -- not applied yet, loop and wait
        showResult cmdStatusMap rId

--  -> OutChan CommandResult
runREPL :: InChan (RequestId, CommandEntry) -> CommandMVarMap -> IO ()
runREPL toCommand cmdStatusMap = do
  cmd <- readPrompt
  case cmd of
    "" -> runREPL toCommand cmdStatusMap
    _ -> do
      cmd' <- return $ BSC.pack cmd
      case readHopper cmd' of
        Left err -> putStrLn cmd >> putStrLn err >> runREPL toCommand cmdStatusMap
        Right _ -> do
            (rId, mvarMap) <- liftIO $ setNextCmdRequestId cmdStatusMap
            writeChan toCommand (rId, CommandEntry cmd')
            showResult mvarMap rId
            runREPL toCommand mvarMap

serverConf :: MonadSnap m => Config m a
serverConf = setErrorLog (ConfigFileLog "log/error.log") $ setAccessLog (ConfigFileLog "log/access.log") defaultConfig

snapServer :: InChan (RequestId, CommandEntry) -> OutChan CommandResult -> CommandMVarMap -> IO ()
snapServer toCommand fromResult cmdStatusMap = httpServe serverConf $
    applyCORS defaultOptions $ methods [GET, POST]
    (ifTop (writeBS "use /hopper for commands") <|>
     route [("accounts/create", createAccounts toCommand cmdStatusMap)
           , ("hopper", hopperHandler toCommand fromResult cmdStatusMap)
           , ("swift", swiftHandler toCommand fromResult cmdStatusMap)
           , ("api/swift-submit", swiftSubmission toCommand fromResult cmdStatusMap)
           , ("api/ledger-query", ledgerQuery toCommand fromResult cmdStatusMap)
           ])

-- |
-- good: curl -H "Content-Type: application/json" -X POST -d '{ "payload" : { "account": "TSLA" }, "digest": { "hash" : "mhash", "key" : "mykey"} }' http://localhost:8000/accounts/create
-- bad: curl -H "Content-Type: application/json" -X POST -d '{ "payloadGarb" : { "account": "KALE" }, "digest": { "hash" : "mhash", "key" : "mykey", "garbage" : "jsonError"} }' http://localhost:8000/accounts/create
createAccounts :: InChan (RequestId, CommandEntry) -> CommandMVarMap -> Snap ()
createAccounts toCommand cmdStatusMap = do
   maybeCreateAccount <- liftM JSON.decode (readRequestBody 10000000)
   case maybeCreateAccount of
     Just (CreateAccountRequest (AccountPayload acct) _) -> do
         (rId, _) <- liftIO $ setNextCmdRequestId cmdStatusMap
         liftIO $ writeChan toCommand (rId, CommandEntry $ createAccountBS' $ T.unpack acct)
         -- byz/client updates successfully
         (writeBS . BL.toStrict . JSON.encode . createAccountResponseSuccess . T.pack) (show rId)
     Nothing -> writeBS . BL.toStrict . JSON.encode $ createAccountResponseFailure "cmdTestFailDecode"
     where
       createAccountBS' acct = BSC.pack $ "CreateAccount " ++ acct

swiftSubmission :: InChan (RequestId, CommandEntry) -> OutChan CommandResult -> CommandMVarMap -> Snap ()
swiftSubmission toCommand fromResult cmdStatusMap = do
  bdy <- readRequestBody 1000000
  cmd <- return $ BLC.toStrict bdy
  logError $ "swiftSubmission: " <> cmd
  let unparsedSwift = decodeUtf8 cmd
  case parseSwift unparsedSwift of
    Left err -> errDone 400 $ BLC.toStrict $ encode $ object ["status" .= ("Failure" :: T.Text), "reason" .= err]
    Right v -> do
      -- TODO: maybe the swift blob should be serialized vs json-ified
      let blob = SwiftBlob unparsedSwift $ swiftToHopper v
      (rId, _) <- liftIO $ setNextCmdRequestId cmdStatusMap
      liftIO $ writeChan toCommand (rId, CommandEntry $ BLC.toStrict $ encode blob)
      resp <- liftIO $ readChan fromResult
      logError $ "swiftSubmission: " <> unCommandResult resp
      modifyResponse $ setHeader "Content-Type" "application/json"
      writeBS $ unCommandResult resp

ledgerQuery :: InChan (RequestId, CommandEntry) -> OutChan CommandResult -> CommandMVarMap -> Snap ()
ledgerQuery toCommand fromResult cmdStatusMap = do
  mBySwift <- fmap BySwiftId <$> getIntegerParam "tx"
  mBySender <- fmap (ByAcctName Sender) <$> getTextParam "sender"
  mByReceiver <- fmap (ByAcctName Receiver) <$> getTextParam "receiver"
  mByBoth <- fmap (ByAcctName Both) <$> getTextParam "account"
  let query = And $ catMaybes [mBySwift, mBySender, mByReceiver, mByBoth]
  -- TODO: if you are querying the ledger should we wait for the command to be applied here?
  (rId, _) <- liftIO $ setNextCmdRequestId cmdStatusMap
  liftIO $ writeChan toCommand (rId, CommandEntry $ BLC.toStrict $ encode query)
  resp <- liftIO $ readChan fromResult
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeBS $ unCommandResult resp

  where
    getIntegerParam p = (>>= fmap fst . BSC.readInteger) <$> getQueryParam p
    getTextParam p = fmap decodeUtf8 <$> getQueryParam p

swiftHandler :: InChan (RequestId, CommandEntry) -> OutChan CommandResult -> CommandMVarMap -> Snap ()
swiftHandler toCommand fromResult cmdStatusMap = do
  bdy <- readRequestBody 1000000
  cmd <- return $ BLC.toStrict bdy
  logError $ "swiftHandler: " <> cmd
  case parseSwift $ decodeUtf8 cmd of
    Left err -> errDone 400 $ BLC.toStrict $ encode $ object ["status" .= ("Failure" :: T.Text), "reason" .= err]
    Right v -> do
        (rId, _) <- liftIO $ setNextCmdRequestId cmdStatusMap
        liftIO $ writeChan toCommand (rId, CommandEntry $ BSC.pack (swiftToHopper v))
        resp <- liftIO $ readChan fromResult
        modifyResponse $ setHeader "Content-Type" "application/json"
        logError $ "swiftHandler: SUCCESS: " <> unCommandResult resp
        writeBS $ unCommandResult resp


errDone :: Int -> BSC.ByteString -> Snap ()
errDone c bs = logError bs >> writeBS bs >> withResponse (finishWith . setResponseCode c)

swiftToHopper :: SWIFT -> String
swiftToHopper m = hopperProgram to' from' inter' amt'
  where
    to' :: String
    to' = T.unpack $ view (sCode59a . bcAccount) m
    from' :: String
    from' = T.unpack $ dirtyPickOutAccount50a m
    intermediaries :: [String]
    intermediaries = ["100","101","102","103"]
    branchA2BranchB = intercalate "->" intermediaries
    branchB2BranchA = intercalate "->" (reverse intermediaries)
    det71a = view sCode71A m
    inter' = case det71a of
               Beneficiary -> branchB2BranchA
               _ -> branchA2BranchB
    amt' :: Ratio Int
    amt' = (fromIntegral $ view (sCode32A . vcsSettlementAmount . vWhole) m) + (view (sCode32A . vcsSettlementAmount . vPart) m)
    hopperProgram :: String -> String -> String -> Ratio Int -> String
    hopperProgram t f i a = "transfer(" ++ f ++ "->" ++ i ++ "->" ++ t ++ "," ++ show a ++ ")"

hopperHandler :: InChan (RequestId, CommandEntry) -> OutChan CommandResult -> CommandMVarMap -> Snap ()
hopperHandler toCommand fromResult cmdStatusMap = do
    bdy <- readRequestBody 1000000
    logError $ "hopper: " <> (BLC.toStrict bdy)
    cmd <- return $ BLC.toStrict bdy
    case readHopper cmd of
      Left err -> errDone 400 $ BSC.pack err
      Right _ -> do
        (rId, _) <- liftIO $ setNextCmdRequestId cmdStatusMap
        liftIO $ writeChan toCommand (rId, CommandEntry cmd)
        resp <- liftIO $ readChan fromResult
        writeBS $ unCommandResult resp

-- | Runs a 'Raft nt String String mt'.
-- Simple fixes nt to 'HostPort' and mt to 'String'.
main :: IO ()
main = do
  (toCommand, fromCommand) <- newChan 1
  -- `toResult` is unused. There seem to be API's that use/block on fromResult.
  -- Either we need to kill this channel full stop or `toResult` needs to be used.
  (toResult, fromResult) <- newChan 1
  cmdStatusMap <- newMVar initCommandMap
  void $ CL.fork $ snapServer toCommand fromResult cmdStatusMap
  let -- getEntry :: (IO et)
      getEntry :: IO (RequestId, CommandEntry)
      getEntry = readChan fromCommand
      -- useResult :: (rt -> IO ())
      -- remove useResult using MVar map now
      --useResult :: CommandResult -> IO ()
      --useResult = writeChan toResult
      -- applyFn :: et -> IO rt
      applyFn :: CommandEntry -> IO CommandResult
      applyFn _x = return $ CommandResult "Failure"
  void $ CL.fork $ runClient applyFn getEntry cmdStatusMap
  threadDelay 100000
  runREPL toCommand cmdStatusMap -- removed fromResult
