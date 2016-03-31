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
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Concurrent.MVar
import qualified Control.Concurrent.Lifted as CL
import Control.Monad

import Schwifty.Swift.M105.Types
import Schwifty.Swift.M105.Parser

import Juno.Spec.Simple
import Juno.Runtime.Types (CommandEntry(..),CommandResult(..),CommandStatus(..),RequestId(..))
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

-- wait for the command to be present in the MVar (used by hopperHandler, swiftHandler, etc.)
waitForCommand :: CommandMVarMap -> RequestId -> IO BSC.ByteString
waitForCommand cmdMap rId =
    threadDelay 1000 >> do
      (CommandMap _ m) <- readMVar cmdMap
      case Map.lookup rId m of
        Nothing -> return $ BSC.pack "Sorry, something went wrong."
        Just (CmdApplied (CommandResult x)) ->
          return $ (BSC.pack . show) $ CommandResult x
        Just _ -> -- not applied yet, loop and wait
          waitForCommand cmdMap rId

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
      if take 11 cmd == "batch test:"
      then do
        rId <- liftIO $ setNextCmdRequestId cmdStatusMap
        writeChan toCommand (rId, CommandEntry cmd')
        threadDelay 1000
        runREPL toCommand cmdStatusMap
      else
        case readHopper cmd' of
          Left err -> putStrLn cmd >> putStrLn err >> runREPL toCommand cmdStatusMap
          Right _ -> do
            rId <- liftIO $ setNextCmdRequestId cmdStatusMap
            writeChan toCommand (rId, CommandEntry cmd')
            showResult cmdStatusMap rId
            runREPL toCommand cmdStatusMap

serverConf :: MonadSnap m => Config m a
serverConf = setErrorLog (ConfigFileLog "log/error.log") $ setAccessLog (ConfigFileLog "log/access.log") defaultConfig

snapServer :: InChan (RequestId, CommandEntry) -> CommandMVarMap -> IO ()
snapServer toCommand cmdStatusMap = httpServe serverConf $
    applyCORS defaultOptions $ methods [GET, POST]
    (ifTop (writeBS "use /hopper for commands") <|>
     route [ ("/api/juno/v1/accounts/create", createAccounts toCommand cmdStatusMap)
           , ("/api/juno/v1/accounts/adjust", adjustAccounts toCommand cmdStatusMap)
           , ("/api/juno/v1/poll", pollForResults cmdStatusMap)
           , ("/api/juno/v1/query", ledgerQueryAPI toCommand cmdStatusMap)
           , ("hopper", hopperHandler toCommand cmdStatusMap)
           , ("swift", swiftHandler toCommand cmdStatusMap)
           , ("api/swift-submit", swiftSubmission toCommand cmdStatusMap)
           , ("api/ledger-query", ledgerQuery toCommand cmdStatusMap)
           ])

-- create an account returns cmdId see: juno/jmeter/juno_API_jmeter_test.jmx
createAccounts :: InChan (RequestId, CommandEntry) -> CommandMVarMap -> Snap ()
createAccounts toCommand cmdStatusMap = do
    maybeCreateAccount <- liftM JSON.decode (readRequestBody 1000)
    modifyResponse $ setHeader "Content-Type" "application/json"
    case maybeCreateAccount of
      Just (CreateAccountRequest (AccountPayload acct) _) -> do
        reqestId@(RequestId rId) <- liftIO $ setNextCmdRequestId cmdStatusMap
        liftIO $ writeChan toCommand (reqestId, CommandEntry $ createAccountBS' $ T.unpack acct)
        -- byz/client updates successfully
        (writeBS . BL.toStrict . JSON.encode) $ commandResponseSuccess ((T.pack . show) rId) ""
      Nothing -> writeBS . BL.toStrict . JSON.encode $ commandResponseFailure "" "Malformed input, could not decode input JSON."
  where
    createAccountBS' acct = BSC.pack $ "CreateAccount " ++ acct

-- Adjusts Account (negative, positive) returns cmdIds: juno/jmeter/juno_API_jmeter_test.jmx
adjustAccounts :: InChan (RequestId, CommandEntry) -> CommandMVarMap -> Snap ()
adjustAccounts toCommand cmdStatusMap = do
   maybeAdjustAccount <- liftM JSON.decode (readRequestBody 1000)
   modifyResponse $ setHeader "Content-Type" "application/json"
   case maybeAdjustAccount of
     Just (AccountAdjustRequest (AccountAdjustPayload acct amt) _) -> do
         reqestId@(RequestId rId) <- liftIO $ setNextCmdRequestId cmdStatusMap
         liftIO $ writeChan toCommand (reqestId, CommandEntry $ adjustAccountBS acct amt)
         (writeBS . BL.toStrict . JSON.encode) $ commandResponseSuccess ((T.pack . show) rId) ""
     Nothing -> writeBS . BL.toStrict . JSON.encode $ commandResponseFailure "" "Malformed input, could not decode input JSON."
     where
       adjustAccountBS acct amt = BSC.pack $ "AdjustAccount " ++ T.unpack acct ++ " " ++ show (toRational amt)

--
ledgerQueryAPI :: InChan (RequestId, CommandEntry) -> CommandMVarMap -> Snap ()
ledgerQueryAPI toCommand cmdStatusMap = do
   maybeQuery <- liftM JSON.decode (readRequestBody 100000)
   modifyResponse $ setHeader "Content-Type" "application/json"
   case maybeQuery of
     Just (LedgerQueryRequest (LedgerQueryBody (QueryJson acct tx sender receiver)) _) -> do
         let mByBoth = (ByAcctName Both) <$> acct
         let mSwiftId =  BySwiftId <$> tx
         let mBySender = (ByAcctName Sender) <$> sender
         let mByReceiver = (ByAcctName Receiver) <$> receiver
         reqestId@(RequestId rId) <- liftIO $ setNextCmdRequestId cmdStatusMap
         let query = And $ catMaybes [ mSwiftId, mBySender, mByReceiver, mByBoth ]
         liftIO $ writeChan toCommand (reqestId, CommandEntry $ BLC.toStrict $ encode query)
         (writeBS . BL.toStrict . JSON.encode) $ commandResponseSuccess ((T.pack . show) rId) ""
     Nothing -> writeBS . BL.toStrict . JSON.encode $ commandResponseFailure "" (T.pack $ "Malformed input, could not decode input JSON.")

-- poll for a list of cmdIds, returning the applied results or error
-- see juno/jmeter/juno_API_jmeter_test.jmx
pollForResults :: CommandMVarMap -> Snap ()
pollForResults cmdStatusMap = do
  maybePoll <- liftM JSON.decode (readRequestBody 1000000)
  modifyResponse $ setHeader "Content-Type" "application/json"
  case maybePoll of
    Just (PollPayloadRequest (PollPayload cmdids) _) -> do
      (CommandMap _ m) <- liftIO $ readMVar cmdStatusMap
      let rids = fmap (RequestId . read . T.unpack) cmdids
      let results = PollResponse $ fmap (toRepresentation . flipIt m) rids
      writeBS . BL.toStrict $ JSON.encode results
    Nothing -> writeBS . BL.toStrict . JSON.encode $ commandResponseFailure "" "Malformed input, could not decode input JSON."
 where

   flipIt :: Map RequestId CommandStatus ->  RequestId -> Maybe (RequestId, CommandStatus)
   flipIt m rId = (fmap . fmap) (\cmd -> (rId, cmd)) (`Map.lookup` m) rId

   toRepresentation :: Maybe (RequestId, CommandStatus) -> PollResult
   toRepresentation (Just (RequestId rid, cmdStatus)) =
       cmdStatus2PollResult (RequestId rid) cmdStatus
   toRepresentation Nothing = cmdStatusError

swiftSubmission :: InChan (RequestId, CommandEntry) -> CommandMVarMap -> Snap ()
swiftSubmission toCommand cmdStatusMap = do
  bdy <- readRequestBody 1000000
  cmd <- return $ BLC.toStrict bdy
  logError $ "swiftSubmission: " <> cmd
  let unparsedSwift = decodeUtf8 cmd
  case parseSwift unparsedSwift of
    Left err -> errDone 400 $ BLC.toStrict $ encode $ object ["status" .= ("Failure" :: T.Text), "reason" .= err]
    Right v -> do
      -- TODO: maybe the swift blob should be serialized vs json-ified
      let blob = SwiftBlob unparsedSwift $ swiftToHopper v
      rId <- liftIO $ setNextCmdRequestId cmdStatusMap
      liftIO $ writeChan toCommand (rId, CommandEntry $ BLC.toStrict $ encode blob)
      resp <- liftIO $ waitForCommand cmdStatusMap rId
      logError $ "swiftSubmission: " <> resp
      modifyResponse $ setHeader "Content-Type" "application/json"
      writeBS resp

ledgerQuery :: InChan (RequestId, CommandEntry) -> CommandMVarMap -> Snap ()
ledgerQuery toCommand cmdStatusMap = do
  mBySwift <- fmap BySwiftId <$> getIntegerParam "tx"
  mBySender <- fmap (ByAcctName Sender) <$> getTextParam "sender"
  mByReceiver <- fmap (ByAcctName Receiver) <$> getTextParam "receiver"
  mByBoth <- fmap (ByAcctName Both) <$> getTextParam "account"
  let query = And $ catMaybes [mBySwift, mBySender, mByReceiver, mByBoth]
  -- TODO: if you are querying the ledger should we wait for the command to be applied here?
  rId <- liftIO $ setNextCmdRequestId cmdStatusMap
  liftIO $ writeChan toCommand (rId, CommandEntry $ BLC.toStrict $ encode query)
  resp <- liftIO $ waitForCommand cmdStatusMap rId
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeBS resp

  where
    getIntegerParam p = (>>= fmap fst . BSC.readInteger) <$> getQueryParam p
    getTextParam p = fmap decodeUtf8 <$> getQueryParam p

swiftHandler :: InChan (RequestId, CommandEntry) -> CommandMVarMap -> Snap ()
swiftHandler toCommand cmdStatusMap = do
  bdy <- readRequestBody 1000000
  cmd <- return $ BLC.toStrict bdy
  logError $ "swiftHandler: " <> cmd
  case parseSwift $ decodeUtf8 cmd of
    Left err -> errDone 400 $ BLC.toStrict $ encode $ object ["status" .= ("Failure" :: T.Text), "reason" .= err]
    Right v -> do
        rId <- liftIO $ setNextCmdRequestId cmdStatusMap
        liftIO $ writeChan toCommand (rId, CommandEntry $ BSC.pack (swiftToHopper v))
        resp <- liftIO $ waitForCommand cmdStatusMap rId
        modifyResponse $ setHeader "Content-Type" "application/json"
        logError $ "swiftHandler: SUCCESS: " <> resp
        writeBS resp


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
    amt' = fromIntegral (view (sCode32A . vcsSettlementAmount . vWhole) m) + view (sCode32A . vcsSettlementAmount . vPart) m
    hopperProgram :: String -> String -> String -> Ratio Int -> String
    hopperProgram t f i a = "transfer(" ++ f ++ "->" ++ i ++ "->" ++ t ++ "," ++ show a ++ ")"

hopperHandler :: InChan (RequestId, CommandEntry) -> CommandMVarMap -> Snap ()
hopperHandler toCommand cmdStatusMap = do
    bdy <- readRequestBody 1000000
    logError $ "hopper: " <> BLC.toStrict bdy
    cmd <- return $ BLC.toStrict bdy
    case readHopper cmd of
      Left err -> errDone 400 $ BSC.pack err
      Right _ -> do
        rId <- liftIO $ setNextCmdRequestId cmdStatusMap
        liftIO $ writeChan toCommand (rId, CommandEntry cmd)
        resp <- liftIO $ waitForCommand cmdStatusMap rId
        writeBS resp

-- | Runs a 'Raft nt String String mt'.
-- Simple fixes nt to 'HostPort' and mt to 'String'.
main :: IO ()
main = do
  (toCommand, fromCommand) <- newChan 1
  -- `toResult` is unused. There seem to be API's that use/block on fromResult.
  -- Either we need to kill this channel full stop or `toResult` needs to be used.
  cmdStatusMap <- initCommandMap
  void $ CL.fork $ snapServer toCommand cmdStatusMap
  let -- getEntry :: (IO et)
      getEntry :: IO (RequestId, CommandEntry)
      getEntry = readChan fromCommand
      -- applyFn :: et -> IO rt
      applyFn :: CommandEntry -> IO CommandResult
      applyFn _x = return $ CommandResult "Failure"
  void $ CL.fork $ runClient applyFn getEntry cmdStatusMap
  threadDelay 100000
  runREPL toCommand cmdStatusMap
