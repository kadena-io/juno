{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Apps.Juno.JsonTypes where

import           Control.Monad (mzero)
import           Data.Aeson ( genericParseJSON
                            , genericToJSON
                            , parseJSON
                            , toJSON
                            , ToJSON
                            , FromJSON
                            )
import           Data.Aeson.Types (defaultOptions
                                  ,object
                                  ,Options(..)
                                  ,(.:)
                                  ,(.=)
                                  )
import           Data.Aeson (Value(..))
import qualified Data.Text as T
import           Data.Text (Text)
import           GHC.Generics
import           Juno.Runtime.Types (CommandStatus(..),
                                     RequestId(..)
                                    )

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
    toJSON (AccountAdjustRequest payload digest) = object ["payload" .= payload, "digest" .= digest]
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
    toJSON (PollPayloadRequest payload digest) = object ["payload" .= payload, "digest" .= digest]
instance FromJSON PollPayloadRequest where
    parseJSON (Object v) = PollPayloadRequest <$>
                             v .: "payload" <*>
                             v .: "digest"
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

data PollResult = PollResult { _pollStatus :: Text
                             , _pollCmdId :: Text
                             , _logidx :: Int
                             , _pollMessage :: Text
                             , _pollResPayload :: Text
                             } deriving (Eq, Generic, Show, FromJSON)
instance ToJSON PollResult where
    toJSON (PollResult status cmdid logidx msg payload) = object [
                                                           "status" .= status,
                                                           "cmdid" .= cmdid,
                                                           "logidx" .= logidx,
                                                           "message" .= msg,
                                                           "payload" .= payload

                                                        ]

-- TODO: logindex, payload after Query/Observe Accounts is added.
cmdStatus2PollResult :: RequestId -> CommandStatus -> PollResult
cmdStatus2PollResult (RequestId rid) CmdSubmitted = PollResult {
                                                      _pollStatus = "PENDING"
                                                    , _pollCmdId = toText rid
                                                    , _logidx = (-1)
                                                    , _pollMessage = ""
                                                    , _pollResPayload = ""
                                                    }
cmdStatus2PollResult (RequestId rid) CmdAccepted = PollResult {
                                                      _pollStatus = "PENDING"
                                                    , _pollCmdId = toText rid
                                                    , _logidx = (-1)
                                                    , _pollMessage = ""
                                                    , _pollResPayload = ""
                                                    }
cmdStatus2PollResult (RequestId rid) (CmdApplied _) = PollResult {
                                                      _pollStatus = "ACCEPTED"
                                                    , _pollCmdId = toText rid
                                                    , _logidx = (-1)
                                                    , _pollMessage = ""
                                                    , _pollResPayload = ""
                                                    }
cmdStatusError :: PollResult
cmdStatusError = PollResult {
                   _pollStatus = "ERROR"
                 , _pollCmdId = "errorID"
                 , _logidx = (-1)
                 , _pollMessage = "nothing to say"
                 , _pollResPayload = "no payload"
                 }

toText :: Show a => a -> Text
toText = (T.pack . show)

data PollResponse = PollResponse { _results :: [PollResult] }
                    deriving (Eq, Generic, Show)

instance ToJSON PollResponse where
    toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = removeUnderscore }
