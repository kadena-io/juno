{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

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
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text as T
import Snap.Http.Server
import Snap.Core
import Control.Lens hiding ((.=))
import Data.Ratio
import Data.Aeson (encode, object, (.=))
import qualified Data.Aeson as JSON

import Juno.Spec.Simple
import Juno.Runtime.Types (CommandEntry(..), CommandResult(..))

import Schwifty.Swift.M105.Types
import Schwifty.Swift.M105.Parser

import Apps.Juno.Parser
import Apps.Juno.Ledger

import Snap.CORS
import Data.List
import Data.Monoid

import qualified Control.Concurrent.Lifted as CL
import Control.Monad
import Apps.Juno.JsonTypes

prompt :: String
prompt = "\ESC[0;31mhopper>> \ESC[0m"

promptGreen :: String
promptGreen = "\ESC[0;32mresult>> \ESC[0m"

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: IO String
readPrompt = flushStr prompt >> getLine

showResult :: OutChan CommandResult -> IO ()
showResult fromResult = readChan fromResult >>= \(CommandResult x) -> putStrLn $ promptGreen ++ BSC.unpack x

runREPL :: InChan CommandEntry -> OutChan CommandResult -> IO ()
runREPL toCommand fromResult = do
  cmd <- readPrompt
  case cmd of
    "" -> runREPL toCommand fromResult
    _ -> do
      cmd' <- return $ BSC.pack cmd
      case readHopper cmd' of
        Left err -> putStrLn cmd >> putStrLn err >> runREPL toCommand fromResult
        Right _ -> writeChan toCommand (CommandEntry cmd') >> showResult fromResult >> runREPL toCommand fromResult

serverConf :: MonadSnap m => Config m a
serverConf = setErrorLog (ConfigFileLog "log/error.log") $ setAccessLog (ConfigFileLog "log/access.log") defaultConfig

snapServer :: InChan CommandEntry -> OutChan CommandResult -> IO ()
snapServer toCommand fromResult = httpServe serverConf $
    applyCORS defaultOptions $ methods [GET, POST]
    (ifTop (writeBS "use /hopper for commands") <|>
     route [("accounts/create", createAccounts toCommand fromResult)
           , ("hopper", hopperHandler toCommand fromResult)
           , ("swift", swiftHandler toCommand fromResult)
           , ("api/swift-submit", swiftSubmission toCommand fromResult)
           , ("api/ledger-query", ledgerQuery toCommand fromResult)])

-- |
-- good: curl -H "Content-Type: application/json" -X POST -d '{ "payload" : { "account": "TSLA" }, "digest": { "hash" : "mhash", "key" : "mykey"} }' http://localhost:8000/accounts/create
-- bad: curl -H "Content-Type: application/json" -X POST -d '{ "payloadGarb" : { "account": "KALE" }, "digest": { "hash" : "mhash", "key" : "mykey", "garbage" : "jsonError"} }' http://localhost:8000/accounts/create
createAccounts :: InChan CommandEntry -> OutChan CommandResult -> Snap ()
createAccounts toCommand fromResult = do
   maybeCreateAccount <- liftM (JSON.decode) (readRequestBody 10000000)
   case maybeCreateAccount of
     Just (CreateAccountRequest (AccountPayload acct) _) -> do
         liftIO $ writeChan toCommand $ CommandEntry $ createAccountBS acct
         _res <- liftIO $ readChan fromResult -- cmdId should be in res?
         (writeBS . BL.toStrict . JSON.encode . createAccountResponseSuccess . T.pack) "cmdIdTODO"

     Nothing ->
         (writeBS . BL.toStrict . JSON.encode) (createAccountResponseFailure "cmdTestFailDecode")

     where
       createAccountBS = (encodeUtf8 . T.pack . show . CreateAccount)

swiftSubmission :: InChan CommandEntry -> OutChan CommandResult -> Snap ()
swiftSubmission toCommand fromResult = do
  bdy <- readRequestBody 1000000
  cmd <- return $ BLC.toStrict bdy
  logError $ "swiftSubmission: " <> cmd
  let unparsedSwift = decodeUtf8 cmd
  case parseSwift unparsedSwift of
    Left err -> errDone 400 $ BLC.toStrict $ encode $ object ["status" .= ("Failure" :: T.Text), "reason" .= err]
    Right v -> do
      -- TODO: maybe the swift blob should be serialized vs json-ified
      let blob = SwiftBlob unparsedSwift $ swiftToHopper v
      liftIO $ writeChan toCommand $ CommandEntry $ BLC.toStrict $ encode blob
      resp <- liftIO $ readChan fromResult
      logError $ "swiftSubmission: " <> unCommandResult resp
      modifyResponse $ setHeader "Content-Type" "application/json"
      writeBS $ unCommandResult resp

ledgerQuery :: InChan CommandEntry -> OutChan CommandResult -> Snap ()
ledgerQuery toCommand fromResult = do
  mBySwift <- fmap BySwiftId <$> getIntegerParam "tx"
  mBySender <- fmap (ByAcctName Sender) <$> getTextParam "sender"
  mByReceiver <- fmap (ByAcctName Receiver) <$> getTextParam "receiver"
  mByBoth <- fmap (ByAcctName Both) <$> getTextParam "account"
  let query = And $ catMaybes [mBySwift, mBySender, mByReceiver, mByBoth]
  liftIO $ writeChan toCommand $ CommandEntry $ BLC.toStrict $ encode query
  resp <- liftIO $ readChan fromResult
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeBS $ unCommandResult resp

  where
    getIntegerParam p = (>>= fmap fst . BSC.readInteger) <$> getQueryParam p
    getTextParam p = fmap decodeUtf8 <$> getQueryParam p

swiftHandler :: InChan CommandEntry -> OutChan CommandResult -> Snap ()
swiftHandler toCommand fromResult = do
  bdy <- readRequestBody 1000000
  cmd <- return $ BLC.toStrict bdy
  logError $ "swiftHandler: " <> cmd
  case parseSwift $ decodeUtf8 cmd of
    Left err -> errDone 400 $ BLC.toStrict $ encode $ object ["status" .= ("Failure" :: T.Text), "reason" .= err]
    Right v -> do
        liftIO $ writeChan toCommand $ CommandEntry $ BSC.pack (swiftToHopper v)
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

hopperHandler :: InChan CommandEntry -> OutChan CommandResult -> Snap ()
hopperHandler toCommand fromResult = do
    bdy <- readRequestBody 1000000
    logError $ "hopper: " <> (BLC.toStrict $ bdy)
    cmd <- return $ BLC.toStrict bdy
    case readHopper cmd of
      Left err -> errDone 400 $ BSC.pack err
      Right _ -> do
        liftIO $ writeChan toCommand $ CommandEntry $ cmd
        resp <- liftIO $ readChan fromResult
        writeBS $ unCommandResult resp

-- | Runs a 'Raft nt String String mt'.
-- Simple fixes nt to 'HostPort' and mt to 'String'.
main :: IO ()
main = do
  (toCommand, fromCommand) <- newChan 1
  (toResult, fromResult) <- newChan 1
  void $ CL.fork $ snapServer toCommand fromResult
  let -- getEntry :: (IO et)
      getEntry :: IO CommandEntry
      getEntry = readChan fromCommand
      -- useResult :: (rt -> IO ())
      useResult :: CommandResult -> IO ()
      useResult = writeChan toResult
      -- applyFn :: et -> IO rt
      applyFn :: CommandEntry -> IO CommandResult
      applyFn _x = return $ CommandResult "Failure"
  void $ CL.fork $ runClient applyFn getEntry useResult
  threadDelay 100000
  runREPL toCommand fromResult
