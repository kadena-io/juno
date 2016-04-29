{-# LANGUAGE OverloadedStrings #-}

module Apps.Juno.ApiHandlers
  ( createAccount
  , adjustAccount
  , transactAPI
  , ledgerQueryAPI
  , cmdBatch
  , apiRoutes
  , ApiEnv(..)
  ) where

import Control.Concurrent.Chan.Unagi
import Control.Concurrent.MVar (readMVar)
import Control.Concurrent.Lifted (threadDelay)
import Control.Lens hiding ((.=))
import Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8)
import Data.Ratio
import Data.Aeson (encode, object, (.=))
import qualified Data.Aeson as JSON
import Snap.Core

import Apps.Juno.JsonTypes
import Apps.Juno.Ledger
import Apps.Juno.Parser
import Juno.Types hiding (CommandBatch)
import Schwifty.Swift.M105.Types
import Schwifty.Swift.M105.Parser

data ApiEnv = ApiEnv {
      _aiToCommands :: InChan (RequestId, [CommandEntry]),
      _aiCmdStatusMap :: CommandMVarMap
}

apiRoutes :: ReaderT ApiEnv Snap ()
apiRoutes = route [
              ("api/juno/v1/accounts/create", createAccount)
             ,("api/juno/v1/accounts/adjust", adjustAccount)
             ,("api/juno/v1/transact", transactAPI)
             ,("api/juno/v1/query", ledgerQueryAPI)
             ,("api/juno/v1/cmd/batch", cmdBatch)
             ,("api/juno/v1/poll", pollForResults)
              -- original API
             ,("hopper", hopperHandler)
             ,("swift", swiftHandler)
             ,("api/swift-submit", swiftSubmission)
             ,("api/ledger-query", ledgerQuery)
             ]

apiWrapper :: (BLC.ByteString -> Either BLC.ByteString [CommandEntry]) -> ReaderT ApiEnv Snap ()
apiWrapper requestHandler = do
   modifyResponse $ setHeader "Content-Type" "application/json"
   reqBytes <- (readRequestBody 1000000)
   case requestHandler reqBytes of
     Right cmdEntries -> do
         env <- ask
         reqestId@(RequestId rId) <- liftIO $ setNextCmdRequestId (_aiCmdStatusMap env)
         liftIO $ writeChan (_aiToCommands env) (reqestId, cmdEntries)
         (writeBS . BLC.toStrict . JSON.encode) $ commandResponseSuccess ((T.pack . show) rId) ""
     Left err -> writeBS $ BLC.toStrict err

-- create an account returns cmdId see: juno/jmeter/juno_API_jmeter_test.jmx
createAccount :: ReaderT ApiEnv Snap ()
createAccount = apiWrapper createAccountReqHandler

-- Adjusts Account (negative, positive) returns cmdIds: juno/jmeter/juno_API_jmeter_test.jmx
adjustAccount :: ReaderT ApiEnv Snap ()
adjustAccount = apiWrapper adjustAccoutReqHandler

-- juno/jmeter/juno_API_jmeter_test.jmx
-- accept hopercommands transfer(000->100->101->102->103->003, 100%1)
transactAPI :: ReaderT ApiEnv Snap ()
transactAPI = apiWrapper transactReqHandler

ledgerQueryAPI :: ReaderT ApiEnv Snap ()
ledgerQueryAPI = apiWrapper ledgerQueryReqHandler

-- receives a list of commands
-- {"payload":{"cmds": ["{\"account\":\"WATER\"}", "{\"account\":\"TSLA\"}"},"digest":{"hash":"hashy","key":"mykey"}}
cmdBatch :: ReaderT ApiEnv Snap ()
cmdBatch = apiWrapper cmdBatchHandler


-- | Handlers to deal with the incomming request bytes and either create the correct
--   [ComandEntry] ByteString or the JSON error message ByteString
createAccountReqHandler :: BLC.ByteString -> Either BLC.ByteString [CommandEntry]
createAccountReqHandler bs =
    case JSON.decode bs of
      Just (CreateAccountRequest (AccountPayload acct) _) ->
          Right [CommandEntry $ createAccountBS' $ T.unpack acct]
      Nothing ->
          Left $ JSON.encode $ commandResponseFailure "" "Malformed input, could not decode input JSON."
  where
    createAccountBS' acct = BSC.pack $ "CreateAccount " ++ acct

adjustAccoutReqHandler :: BLC.ByteString -> Either BLC.ByteString [CommandEntry]
adjustAccoutReqHandler bs =
    case JSON.decode bs of
        Just (AccountAdjustRequest (AccountAdjustPayload acct (JRational amt)) _) ->
            Right [CommandEntry $ adjustAccountBS acct amt]
        Nothing ->
            Left $ JSON.encode $ commandResponseFailure "" "Malformed input, could not decode input JSON."
  where
    adjustAccountBS acct amt = BSC.pack $ "AdjustAccount " ++ T.unpack acct ++ " " ++ show (toRational amt)

transactReqHandler :: BLC.ByteString -> Either BLC.ByteString [CommandEntry]
transactReqHandler bs =
    case JSON.decode bs of
      Just (TransactRequest (TransactBody code _body) _) ->
          Right [CommandEntry $ BSC.pack $ T.unpack code]
      Nothing ->
          Left $ JSON.encode $ commandResponseFailure "" "Malformed input, could not decode input JSON."

ledgerQueryReqHandler :: BLC.ByteString -> Either BLC.ByteString [CommandEntry]
ledgerQueryReqHandler bs =
    case JSON.decode bs of
      Just (LedgerQueryRequest (LedgerQueryBody (QueryJson acct tx sender receiver)) _) -> do
         let mByBoth = ByAcctName Both <$> acct
         let mSwiftId =  BySwiftId <$> tx
         let mBySender = ByAcctName Sender <$> sender
         let mByReceiver = ByAcctName Receiver <$> receiver
         let query = And $ catMaybes [mSwiftId, mBySender, mByReceiver, mByBoth]
         Right [CommandEntry $ BLC.toStrict $ encode query]
      Nothing ->
         Left $ JSON.encode $ commandResponseFailure "" "Malformed input, could not decode input JSON."

cmdBatchHandler :: BLC.ByteString -> Either BLC.ByteString [CommandEntry]
cmdBatchHandler bs =
    case JSON.decode bs of
      Just (CommandBatchRequest (CommandBatch cmds) _) ->
        -- or catMaybes depending if one fails, all fail?
        case sequence $ fmap (mJsonBsToCommand . BLC.pack . T.unpack) cmds of
          Just cmds' -> Right $ fmap CommandEntry cmds'
          Nothing -> Left errorBadCommand
      Nothing -> Left errorBadCommandBatch
 where
   errorBadCommand = JSON.encode $ commandResponseFailure "" "Malformed cmd or cmds in the submitted batch, could not decode input JSON."
   errorBadCommandBatch = JSON.encode $ commandResponseFailure "" "Malformed input, could not decode input JSON."

-- TODO: _aiCmdStatusMap needs to be updated by Juno protocol this is never updated
-- poll for a list of cmdIds, returning the applied results or error
-- see juno/jmeter/juno_API_jmeter_test.jmx
pollForResults :: ReaderT ApiEnv Snap ()
pollForResults = do
    maybePoll <- liftM JSON.decode (readRequestBody 1000000)
    modifyResponse $ setHeader "Content-Type" "application/json"
    case maybePoll of
      Just (PollPayloadRequest (PollPayload cmdids) _) -> do
        env <- ask
        (CommandMap _ m) <- liftIO $ readMVar (_aiCmdStatusMap env)
        let rids = (RequestId . read . T.unpack) <$> cleanInput cmdids
        let results = PollResponse $ fmap (toRepresentation . flipIt m) rids
        writeBS . BLC.toStrict $ JSON.encode results
      Nothing -> writeBS . BLC.toStrict . JSON.encode $ commandResponseFailure "" "Malformed input, could not decode input JSON."
  where
    -- for now allow "" (empty) cmdIds
    cleanInput = filter (/=T.empty)

    flipIt :: Map RequestId CommandStatus ->  RequestId -> Maybe (RequestId, CommandStatus)
    flipIt m rId = (fmap . fmap) (\cmd -> (rId, cmd)) (`Map.lookup` m) rId

    toRepresentation :: Maybe (RequestId, CommandStatus) -> PollResult
    toRepresentation (Just (RequestId rid, cmdStatus)) =
        cmdStatus2PollResult (RequestId rid) cmdStatus
    toRepresentation Nothing = cmdStatusError

swiftSubmission :: ReaderT ApiEnv Snap ()
swiftSubmission = do
  (ApiEnv aiToCommands aiCmdStatusMap) <- ask
  bdy <- readRequestBody 1000000
  cmd <- return $ BLC.toStrict bdy
  logError $ "swiftSubmission: " <> cmd
  let unparsedSwift = decodeUtf8 cmd
  case parseSwift unparsedSwift of
    -- TODO: get to work with `errDone 400 $`
    Left err ->
        writeBS $ BLC.toStrict $ encode $
                object ["status" .= ("Failure" :: T.Text), "reason" .= err]
    Right v -> do
      -- TODO: maybe the swift blob should be serialized vs json-ified
      let blob = SwiftBlob unparsedSwift $ swiftToHopper v
      rId <- liftIO $ setNextCmdRequestId aiCmdStatusMap
      liftIO $ writeChan aiToCommands (rId, [CommandEntry $ BLC.toStrict $ encode blob])
      resp <- liftIO $ waitForCommand aiCmdStatusMap rId
      logError $ "swiftSubmission: " <> resp
      modifyResponse $ setHeader "Content-Type" "application/json"
      writeBS resp

getIntegerParam :: MonadSnap f => BSC.ByteString -> f (Maybe Integer)
getIntegerParam p = (>>= fmap fst . BSC.readInteger) <$> getQueryParam p

getTextParam :: MonadSnap f => BSC.ByteString -> f (Maybe T.Text)
getTextParam p = fmap decodeUtf8 <$> getQueryParam p

ledgerQuery :: ReaderT ApiEnv Snap ()
ledgerQuery = do
  (ApiEnv aiToCommands aiCmdStatusMap) <- ask
  mBySwift <- fmap BySwiftId <$> getIntegerParam "tx"
  mBySender <- fmap (ByAcctName Sender) <$> getTextParam "sender"
  mByReceiver <- fmap (ByAcctName Receiver) <$> getTextParam "receiver"
  mByBoth <- fmap (ByAcctName Both) <$> getTextParam "account"
  let query = And $ catMaybes [mBySwift, mBySender, mByReceiver, mByBoth]
  -- TODO: if you are querying the ledger should we wait for the command to be applied here?
  rId <- liftIO $ setNextCmdRequestId aiCmdStatusMap
  liftIO $ writeChan aiToCommands (rId, [CommandEntry $ BLC.toStrict $ encode query])
  resp <- liftIO $ waitForCommand aiCmdStatusMap rId
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeBS resp

swiftHandler :: ReaderT ApiEnv Snap ()
swiftHandler = do
  (ApiEnv aiToCommands aiCmdStatusMap) <- ask
  bdy <- readRequestBody 1000000
  cmd <- return $ BLC.toStrict bdy
  logError $ "swiftHandler: " <> cmd
  case parseSwift $ decodeUtf8 cmd of
     -- TODO: get to work with `errDone 400 $`
    Left err -> writeBS $ BLC.toStrict $ encode $
                object ["status" .= ("Failure" :: T.Text), "reason" .= err]
    Right v -> do
        rId <- liftIO $ setNextCmdRequestId aiCmdStatusMap
        liftIO $ writeChan aiToCommands (rId, [CommandEntry $ BSC.pack (swiftToHopper v)])
        resp <- liftIO $ waitForCommand aiCmdStatusMap rId
        modifyResponse $ setHeader "Content-Type" "application/json"
        logError $ "swiftHandler: SUCCESS: " <> resp
        writeBS resp

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

hopperHandler :: ReaderT ApiEnv Snap ()
hopperHandler = do
  (ApiEnv aiToCommands aiCmdStatusMap) <- ask
  bdy <- readRequestBody 1000000
  logError $ "hopper: " <> BLC.toStrict bdy
  cmd <- return $ BLC.toStrict bdy
  case readHopper cmd of
    -- TODO: get to work with `errDone 400 $`
    Left err -> writeBS $ BSC.pack err
    Right _ -> do
      rId <- liftIO $ setNextCmdRequestId aiCmdStatusMap
      liftIO $ writeChan aiToCommands (rId, [CommandEntry cmd])
      resp <- liftIO $ waitForCommand aiCmdStatusMap rId
      writeBS resp

-- wait for the command to be present in the MVar (used by hopperHandler, swiftHandler, etc.)
waitForCommand :: CommandMVarMap -> RequestId -> IO BSC.ByteString
waitForCommand cmdMap rId =
  threadDelay 1000 >> do
    (CommandMap _ m) <- readMVar cmdMap
    case Map.lookup rId m of
      Nothing -> return $ BSC.pack $ "RequestId [" ++ show rId ++ "] not found."
      Just (CmdApplied (CommandResult bs) _lat) ->
        return bs
      Just _ -> -- not applied yet, loop and wait
        waitForCommand cmdMap rId
