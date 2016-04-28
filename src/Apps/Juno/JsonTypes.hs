{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Apps.Juno.JsonTypes where

import           Control.Monad (mzero)
import           Data.Aeson as JSON
import           Data.Text.Encoding (decodeUtf8)
import           Data.Aeson.Types (Options(..),defaultOptions,parseMaybe)
import qualified Data.Text as T
import           Data.Text (Text)
import           GHC.Generics
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Control.Applicative

import           Juno.Types (CommandStatus(..),CommandResult(..),RequestId(..))

removeUnderscore :: String -> String
removeUnderscore = drop 1

data Digest = Digest { _hash :: Text, _key :: Text } deriving (Eq, Generic, Show)

instance ToJSON Digest where
    toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = removeUnderscore }
instance FromJSON Digest where
    parseJSON  = genericParseJSON $ defaultOptions { fieldLabelModifier = removeUnderscore }


data AccountPayload = AccountPayload { _account :: Text } deriving (Eq, Generic, Show)

instance ToJSON AccountPayload where
    toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = removeUnderscore }
instance FromJSON AccountPayload where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = removeUnderscore }


data CreateAccountRequest = CreateAccountRequest {
      _payload :: AccountPayload,
      _digest :: Digest
    } deriving (Eq, Generic, Show)

instance ToJSON CreateAccountRequest where
    toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = removeUnderscore }
instance FromJSON CreateAccountRequest where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = removeUnderscore }

data CommandResponse = CommandResponse {
      _status :: Text
    , _cmdid :: Text
    , _message :: Text
    } deriving (Eq, Generic, Show)

instance ToJSON CommandResponse where
    toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = removeUnderscore }
instance FromJSON CommandResponse where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = removeUnderscore }

commandResponseSuccess :: Text -> Text -> CommandResponse
commandResponseSuccess cid msg = CommandResponse "Success" cid msg

commandResponseFailure :: Text -> Text -> CommandResponse
commandResponseFailure cid msg = CommandResponse "Failure" cid msg

-- | AccountAdjust adding/substracting money from and existing account
-- { "payload": { "account": "TSLA", "amount": 100.0 }, "digest": { "hash": "myhash", "key": "string" } }
data AccountAdjustPayload = AccountAdjustPayload {
      _adjustAccount :: Text
    , _adjustAmount :: Double } deriving (Eq, Generic, Show)

instance ToJSON AccountAdjustPayload where
    toJSON (AccountAdjustPayload account amount) = object ["account" .= account, "amount" .= amount]
instance FromJSON AccountAdjustPayload where
    parseJSON (Object v) = AccountAdjustPayload <$>
                             v .: "account" <*>
                             v .: "amount"
    parseJSON _ = mzero

data AccountAdjustRequest = AccountAdjustRequest {
      _adjustAccountPayload :: AccountAdjustPayload,
      _adjustAccountDigest :: Digest
    } deriving (Eq, Generic, Show)

instance ToJSON AccountAdjustRequest where
    toJSON (AccountAdjustRequest payload' digest') = object ["payload" .= payload', "digest" .= digest']
instance FromJSON AccountAdjustRequest where
    parseJSON (Object v) = AccountAdjustRequest <$>
                             v .: "payload" <*>
                             v .: "digest"
    parseJSON _ = mzero

-- | Polling for commands
data PollPayload = PollPayload {
  _cmdids :: [Text]
  } deriving (Eq, Generic, Show)

instance ToJSON PollPayload where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = removeUnderscore }
instance FromJSON PollPayload where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = removeUnderscore }

data PollPayloadRequest = PollPayloadRequest {
  _pollPayload :: PollPayload,
  _pollDigest :: Digest
  } deriving (Eq, Generic, Show)

instance ToJSON PollPayloadRequest where
  toJSON (PollPayloadRequest payload' digest') = object ["payload" .= payload', "digest" .= digest']
instance FromJSON PollPayloadRequest where
  parseJSON (Object v) = PollPayloadRequest <$> v .: "payload"
                                            <*> v .: "digest"
  parseJSON _ = mzero

-- {
--  "results": [
--    {
--      "status": "PENDING",
--      "cmdid": "string",
--      "logidx": "string",
--      "message": "string",
--      "payload": {}
--    }
--  ]
-- }

data PollResult = PollResult
  { _pollStatus :: Text
  , _pollCmdId :: Text
  , _logidx :: Int
  , _pollMessage :: Text
  , _pollResPayload :: Text
  } deriving (Eq, Generic, Show, FromJSON)
instance ToJSON PollResult where
    toJSON (PollResult status cmdid logidx msg payload') =
      object [ "status" .= status
             , "cmdid" .= cmdid
             , "logidx" .= logidx
             , "message" .= msg
             , "payload" .= payload'
             ]

-- TODO: logindex, payload after Query/Observe Accounts is added.
cmdStatus2PollResult :: RequestId -> CommandStatus -> PollResult
cmdStatus2PollResult (RequestId rid) CmdSubmitted =
  PollResult{_pollStatus = "PENDING", _pollCmdId = toText rid, _logidx = -1, _pollMessage = "", _pollResPayload = ""}
cmdStatus2PollResult (RequestId rid) CmdAccepted =
  PollResult{_pollStatus = "PENDING", _pollCmdId = toText rid, _logidx = -1, _pollMessage = "", _pollResPayload = ""}
cmdStatus2PollResult (RequestId rid) (CmdApplied (CommandResult res) _) =
  PollResult{_pollStatus = "ACCEPTED", _pollCmdId = toText rid, _logidx = -1, _pollMessage = "", _pollResPayload = decodeUtf8 res}

cmdStatusError :: PollResult
cmdStatusError = PollResult
  { _pollStatus = "ERROR"
  , _pollCmdId = "errorID"
  , _logidx = -1
  , _pollMessage = "nothing to say"
  , _pollResPayload = "no payload"
  }

toText :: Show a => a -> Text
toText = T.pack . show

data PollResponse = PollResponse { _results :: [PollResult] }
  deriving (Eq, Generic, Show)

instance ToJSON PollResponse where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = removeUnderscore }

-- {
-- "payload": {
--   "filter": "string"
-- },
-- "digest": {
--   "hash": "string",
--   "key": "string"
-- }
data QueryJson = QueryJson {  _queryAcct :: Maybe Text
                            , _queryTx :: Maybe Integer
                            , _querySender :: Maybe Text
                            , _queryReceiver :: Maybe Text
                            } deriving (Eq, Generic, Show)
instance ToJSON QueryJson where
  toJSON (QueryJson acct tx sender receiver) = object [
                                                "account" .= Just acct
                                               , "tx" .= Just tx
                                               , "sender" .= Just sender
                                               , "receiver" .= Just receiver
                                               ]
instance FromJSON QueryJson where
  parseJSON (Object v) = QueryJson <$> v .: "account"
                                   <*> v .: "tx"
                                   <*> v .: "sender"
                                   <*> v .: "receiver"
  parseJSON _ = mzero


data LedgerQueryBody = LedgerQueryBody { _filter :: QueryJson }
                       deriving (Show, Generic, Eq)

instance ToJSON LedgerQueryBody where
    toJSON (LedgerQueryBody fil) = object ["filter" .= fil]
instance FromJSON LedgerQueryBody where
     parseJSON (Object v) = LedgerQueryBody <$> v .: "filter"
     parseJSON _ = mzero

data LedgerQueryRequest = LedgerQueryRequest {
      payload :: LedgerQueryBody,
      digest :: Digest
    } deriving (Show, Generic, Eq)
instance ToJSON LedgerQueryRequest where
  toJSON (LedgerQueryRequest payload' digest') = object ["payload" .= payload', "digest" .= digest']
instance FromJSON LedgerQueryRequest where
  parseJSON (Object v) = LedgerQueryRequest <$> v .: "payload"
                                            <*> v .: "digest"
  parseJSON _ = mzero

-- Transact
--
-- "payload": {
--   "code": "string",
--   "data": {}
-- },
-- "digest": {
--   "hash": "string",
--   "key": "string"
-- }

data TransactBody = TransactBody { _txCode :: Text, _txData :: Text } deriving (Show, Eq)
instance ToJSON TransactBody where
    toJSON (TransactBody code txData) = object ["code" .= code, "data" .= txData]
instance FromJSON TransactBody where
    parseJSON (Object v) = TransactBody <$> v .: "code"
                                        <*> v .: "data"
    parseJSON _ = mzero

data TransactRequest = TransactRequest { _txPayload :: TransactBody
                                       , _txDigest :: Digest
                                       } deriving (Show, Eq)
instance ToJSON TransactRequest where
    toJSON (TransactRequest payload' digest') = object ["payload" .= payload'
                                                       , "digest" .= digest']
instance FromJSON TransactRequest where
    parseJSON (Object v) = TransactRequest <$> v .: "payload"
                                           <*> v .: "digest"
    parseJSON _ = mzero

-- {
-- "payload": {
--   "cmds": ["string"]
-- },
-- "digest": {
--   "hash": "string",
--   "key": "string"
-- }
data CommandBatch = CommandBatch {
      _batchCmds :: [Text]
    } deriving (Eq,Show,Ord)

instance ToJSON CommandBatch where
    toJSON (CommandBatch cmds) = object ["cmds" .= cmds]
instance FromJSON CommandBatch where
     parseJSON (Object v) = CommandBatch <$> v .: "cmds"
     parseJSON _ = mzero

data CommandBatchRequest = CommandBatchRequest {
      cmdBatchpayload :: CommandBatch,
      cmdBatchdigest :: Digest
    } deriving (Show, Generic, Eq)

instance ToJSON CommandBatchRequest where
  toJSON (CommandBatchRequest payload' digest') = object ["payload" .= payload', "digest" .= digest']
instance FromJSON CommandBatchRequest where
  parseJSON (Object v) = CommandBatchRequest <$> v .: "payload"
                                             <*> v .: "digest"
  parseJSON _ = mzero

mJsonBsToCommand :: BLC.ByteString -> Maybe BSC.ByteString
mJsonBsToCommand bs = JSON.decode bs >>= \v ->
                      (mAdjustAccount <$> tryParse v) <|>
                      (mAdjustCreateAcct <$> tryParse v) <|>
                      (mtransact <$> tryParse v)
  where

    tryParse v = parseMaybe parseJSON v

    mAdjustAccount (AccountAdjustPayload acct amt) =
        BSC.pack $ "AdjustAccount " ++  T.unpack acct ++ " " ++ show (toRational amt)

    mAdjustCreateAcct (AccountPayload acct) =
        BSC.pack $ "CreateAccount " ++  T.unpack acct

    mtransact (TransactBody code _) =
        BSC.pack $ T.unpack code
