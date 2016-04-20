{-# LANGUAGE OverloadedStrings #-}

module Apps.Juno.ApiHandlers (
                            createAccount
                           ,adjustAccount
                           ,transactAPI
                           ,ledgerQueryAPI
                           ,cmdBatch
                           ,apiRoutes
                           ,ApiEnv(..)
                           ) where

import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.MVar (readMVar)
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import           Data.Maybe (catMaybes)
import qualified Data.Map as Map
import           Data.Map (Map)

import           Snap.Core
import           Data.Aeson (encode)
import qualified Data.Aeson as JSON

import           Apps.Juno.JsonTypes
import           Juno.Runtime.Types hiding (CommandBatch)
import           Juno.Consensus.ByzRaft.Client (
                                                CommandMVarMap
                                               ,setNextCmdRequestId
                                               )


import           Apps.Juno.Ledger
import           Control.Monad.Reader
import           Juno.Consensus.ByzRaft.Client (CommandMap(..))

data ApiEnv = ApiEnv {
      _toCommands :: InChan (RequestId, [CommandEntry]),
      _cmdStatusMap ::  CommandMVarMap
}

apiRoutes :: ReaderT ApiEnv Snap ()
apiRoutes = route [
              ("/accounts/create", createAccount)
             ,("/accounts/adjust", adjustAccount)
             ,("/transact", transactAPI)
             ,("/query", ledgerQueryAPI)
             ,("/cmd/batch", cmdBatch)
             ,("/poll", pollForResults)
             ]

apiWrapper :: (BLC.ByteString -> Either BLC.ByteString [CommandEntry]) -> ReaderT ApiEnv Snap ()
apiWrapper requestHandler = do
   modifyResponse $ setHeader "Content-Type" "application/json"
   reqBytes <- (readRequestBody 1000000)
   case (requestHandler reqBytes) of
     Right cmdEntries -> do
         env <- ask
         reqestId@(RequestId rId) <- liftIO $ setNextCmdRequestId (_cmdStatusMap env)
         liftIO $ writeChan (_toCommands env) (reqestId, cmdEntries)
         (writeBS . BLC.toStrict . JSON.encode) $ commandResponseSuccess ((T.pack . show) rId) ""
     Left err -> (writeBS . BLC.toStrict) $ err

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
     case (JSON.decode bs) of
       Just (CreateAccountRequest (AccountPayload acct) _) ->
           Right [CommandEntry $ createAccountBS' $ T.unpack acct]
       Nothing ->
           Left $ (JSON.encode $ commandResponseFailure "" "Malformed input, could not decode input JSON.")
 where
   createAccountBS' acct = BSC.pack $ "CreateAccount " ++ acct

adjustAccoutReqHandler :: BLC.ByteString -> Either BLC.ByteString [CommandEntry]
adjustAccoutReqHandler bs =
  case (JSON.decode bs) of
     Just (AccountAdjustRequest (AccountAdjustPayload acct amt) _) ->
         Right [CommandEntry $ adjustAccountBS acct amt]
     Nothing ->
         Left $ (JSON.encode $ commandResponseFailure "" "Malformed input, could not decode input JSON.")
 where
  adjustAccountBS acct amt = BSC.pack $ "AdjustAccount " ++ T.unpack acct ++ " " ++ show (toRational amt)

transactReqHandler :: BLC.ByteString -> Either BLC.ByteString [CommandEntry]
transactReqHandler bs =
    case (JSON.decode bs) of
      Just (TransactRequest (TransactBody code body) _) ->
          Right [CommandEntry $ BSC.pack $ T.unpack code]
      Nothing ->
          Left $ (JSON.encode $ commandResponseFailure "" "Malformed input, could not decode input JSON.")

ledgerQueryReqHandler :: BLC.ByteString -> Either BLC.ByteString [CommandEntry]
ledgerQueryReqHandler bs =
    case (JSON.decode bs) of
      Just (LedgerQueryRequest (LedgerQueryBody (QueryJson acct tx sender receiver)) _) -> do
         let mByBoth = (ByAcctName Both) <$> acct
         let mSwiftId =  BySwiftId <$> tx
         let mBySender = (ByAcctName Sender) <$> sender
         let mByReceiver = (ByAcctName Receiver) <$> receiver
         let query = And $ catMaybes [ mSwiftId, mBySender, mByReceiver, mByBoth ]
         Right $[CommandEntry $ BLC.toStrict $ encode query]
      Nothing ->
         Left $ (JSON.encode $ commandResponseFailure "" "Malformed input, could not decode input JSON.")

cmdBatchHandler :: BLC.ByteString -> Either BLC.ByteString [CommandEntry]
cmdBatchHandler bs =
    case (JSON.decode bs) of
      Just (CommandBatchRequest (CommandBatch cmds) _) ->
        -- or catMaybes depending if one fails, all fail?
        case (sequence $ fmap (mJsonBsToCommand . BLC.pack . T.unpack) cmds) of
          Just cmds' -> Right $ fmap CommandEntry cmds'
          Nothing -> Left errorBadCommand
      Nothing -> Left errorBadCommandBatch
 where
   errorBadCommand = JSON.encode $ commandResponseFailure "" "Malformed cmd or cmds in the submitted batch, could not decode input JSON."
   errorBadCommandBatch = JSON.encode $ commandResponseFailure "" "Malformed input, could not decode input JSON."

-- TODO: _cmdStatusMap needs to be updated by Juno protocol this is never updated
-- poll for a list of cmdIds, returning the applied results or error
-- see juno/jmeter/juno_API_jmeter_test.jmx
pollForResults :: ReaderT ApiEnv Snap ()
pollForResults = do
  maybePoll <- liftM JSON.decode (readRequestBody 1000000)
  modifyResponse $ setHeader "Content-Type" "application/json"
  case maybePoll of
    Just (PollPayloadRequest (PollPayload cmdids) _) -> do
      env <- ask
      (CommandMap _ m) <- liftIO $ readMVar (_cmdStatusMap env)
      let rids = fmap (RequestId . read . T.unpack) $ cleanInput cmdids
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
