{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Apps.Juno.JsonTypes where

import           Control.Monad (mzero)
import           Data.Aeson (encode
                            , decode
                            , genericParseJSON
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
import qualified Data.ByteString.Lazy.Char8 as BL
import           GHC.Generics
import qualified Data.Text as T
import           Data.Text (Text)

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

-- Tests
test :: Bool
test = testCAEncode && testCAEncode && testCADecode'

bytesCreateAccount :: BL.ByteString
bytesCreateAccount = BL.pack "{\"digest\":{\"hash\":\"hashy\",\"key\":\"mykey\"},\"payload\":{\"account\":\"TSLA\"}}"

bytesDigest :: BL.ByteString
bytesDigest = BL.pack "{\"hash\":\"hashy\",\"key\":\"mykey\"}"

testCADecode :: Bool
testCADecode =  (decode bytesCreateAccount :: Maybe CreateAccountRequest) == (Just (CreateAccountRequest {_payload = AccountPayload {_account = "TSLA"}, _digest = Digest {_hash = "hashy", _key = "mykey"}}))

testCADecode' :: Bool
testCADecode' = case (decode bytesCreateAccount :: Maybe CreateAccountRequest) of
                  Just (CreateAccountRequest (AccountPayload _) _) -> True
                  Nothing -> False

testDecodeDigest :: Bool
testDecodeDigest = case (decode bytesDigest :: Maybe Digest) of
                     Just (Digest _ _) -> True
                     Nothing -> False
testCAEncode :: Bool
testCAEncode = (encode (CreateAccountRequest (AccountPayload (T.pack "TSLA")) (Digest (T.pack "hashy") (T.pack "mykey")))) == ("{\"payload\":{\"account\":\"TSLA\"},\"digest\":{\"hash\":\"hashy\",\"key\":\"mykey\"}}")

-- | Test account adjust

testAdjustPayloadDecode :: Bool
testAdjustPayloadDecode = (decode $ BL.pack "{\"amount\":100,\"account\":\"TSLA\"}" :: Maybe AccountAdjustPayload) ==
                          Just (AccountAdjustPayload {_adjustAccount = "TSLA", _adjustAmount = 100.0})

testAdjustPayloadEncode :: Bool
testAdjustPayloadEncode = (encode $ AccountAdjustPayload "TSLA" 100) == "{\"amount\":100,\"account\":\"TSLA\"}"

bytesAdjustRequestPayload :: BL.ByteString
bytesAdjustRequestPayload =  BL.pack "{\"payload\":{\"amount\":100,\"account\":\"TSLA\"},\"digest\":{\"hash\":\"hashy\",\"key\":\"mykey\"}}"

testAdjustRequestEncode :: BL.ByteString
testAdjustRequestEncode = encode $
                          AccountAdjustRequest (AccountAdjustPayload (T.pack "TSLA") 100)
                                        (Digest (T.pack "hashy") (T.pack "mykey"))

testAdjustRequestDecode :: Bool
testAdjustRequestDecode = (decode $ bytesAdjustRequestPayload :: Maybe AccountAdjustRequest) ==
                   Just (
                         AccountAdjustRequest {
                           _adjustAccountPayload = AccountAdjustPayload
                           {_adjustAccount = "TSLA", _adjustAmount = 100.0}
                         , _adjustAccountDigest = Digest {_hash = "hashy", _key = "mykey"}
                         }
                        )
