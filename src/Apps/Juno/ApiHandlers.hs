{-# LANGUAGE OverloadedStrings #-}

module Apps.Juno.ApiHandlers (
                            createAccount
                           ,adjustAccount
                           ,transactAPI
                           ,ledgerQueryAPI
                           ,cmdBatch
                           ) where

import           Control.Concurrent.Chan.Unagi
import           Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import           Data.Maybe (catMaybes)

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

apiWrapper :: InChan (RequestId, [CommandEntry]) -> CommandMVarMap ->  (BLC.ByteString -> Either BLC.ByteString [CommandEntry]) -> Snap ()
apiWrapper toCommands cmdStatusMap requestHandler = do
   modifyResponse $ setHeader "Content-Type" "application/json"
   reqBytes <- (readRequestBody 1000000)
   case (requestHandler reqBytes) of
     Right cmdEntries -> do
         reqestId@(RequestId rId) <- liftIO $ setNextCmdRequestId cmdStatusMap
         liftIO $ writeChan toCommands (reqestId, cmdEntries)
         (writeBS . BLC.toStrict . JSON.encode) $ commandResponseSuccess ((T.pack . show) rId) ""
     Left err -> (writeBS . BLC.toStrict) $ err

-- create an account returns cmdId see: juno/jmeter/juno_API_jmeter_test.jmx
createAccount :: InChan (RequestId, [CommandEntry]) -> CommandMVarMap -> Snap ()
createAccount toCommands cmdStatusMap =
    apiWrapper toCommands cmdStatusMap createAccountReqHandler

-- Adjusts Account (negative, positive) returns cmdIds: juno/jmeter/juno_API_jmeter_test.jmx
adjustAccount :: InChan (RequestId, [CommandEntry]) -> CommandMVarMap -> Snap ()
adjustAccount toCommands cmdStatusMap =
    apiWrapper toCommands cmdStatusMap adjustAccoutReqHandler

-- juno/jmeter/juno_API_jmeter_test.jmx
-- accept hopercommands transfer(000->100->101->102->103->003, 100%1)
transactAPI :: InChan (RequestId, [CommandEntry]) -> CommandMVarMap -> Snap ()
transactAPI toCommands cmdStatusMap =
   apiWrapper toCommands cmdStatusMap transactReqHandler

ledgerQueryAPI :: InChan (RequestId, [CommandEntry]) -> CommandMVarMap -> Snap ()
ledgerQueryAPI toCommands cmdStatusMap =
    apiWrapper toCommands cmdStatusMap ledgerQueryReqHandler

-- receives a list of commands
-- {"payload":{"cmds": ["{\"account\":\"WATER\"}", "{\"account\":\"TSLA\"}"},"digest":{"hash":"hashy","key":"mykey"}}
cmdBatch :: InChan (RequestId, [CommandEntry]) -> CommandMVarMap -> Snap ()
cmdBatch toCommands cmdStatusMap =
    apiWrapper toCommands cmdStatusMap cmdBatchHandler


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
