{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Apps.Juno.JsonModels where

import           Control.Applicative
import           Data.Aeson (encode
                            , decode
                            , parseJSON
                            , toJSON
                            , object
                            , (.:), (.=)
                            , Value (..)
                            , ToJSON
                            , FromJSON)
import qualified Data.ByteString.Lazy.Char8 as BL
import           GHC.Generics
import qualified Data.Text as T
import           Data.Text (Text)

data Digest = Digest { _hash :: Text, _key :: Text } deriving (Eq, Generic, Show)
instance ToJSON Digest where
instance FromJSON Digest where
    parseJSON (Object v) = Digest <$>
                           v .: "hash" <*>
                           v .: "key"
    parseJSON _          = empty

-- | CreateAccountRequest
-- encode (CreateAccoutRequest (AccountPayload "TSLA") (Digest "hashy" "mykey"))
-- "{\"_digest\":{\"_hash\":\"hashy\",\"_key\":\"mykey\"},\"_payload\":{\"_account\":\"TSLA\"}}"
-- decode bytesCreateAccount :: Maybe CreateAccountRequest
data AccountPayload = AccountPayload { _account :: Text } deriving (Eq, Generic, Show)

instance ToJSON AccountPayload where
    toJSON (AccountPayload acct) =
        object ["account" .= acct]

instance FromJSON AccountPayload where
  parseJSON (Object v) = AccountPayload <$>
                         v .: "account"
  parseJSON _          = empty

data CreateAccountRequest = CreateAccountRequest {
      _payload :: AccountPayload,
      _digest :: Digest
    } deriving (Eq, Generic, Show)
instance ToJSON CreateAccountRequest where
instance FromJSON CreateAccountRequest where
     parseJSON (Object v) = CreateAccountRequest <$>
                            v .: "payload" <*>
                            v .: "digest"
     parseJSON _          = empty

data CreateAccountResponse = CreateAccountResponse {
      cmdid :: Text
    , status :: Text
    } deriving (Eq, Generic, Show)
instance ToJSON CreateAccountResponse where
instance FromJSON CreateAccountResponse where

createAccountResponseSuccess :: Text -> CreateAccountResponse
createAccountResponseSuccess cid = CreateAccountResponse cid "Success"

createAccountResponseFailure :: Text -> CreateAccountResponse
createAccountResponseFailure cid = CreateAccountResponse cid "Failure"

-- Tests
test :: Bool
test = testCAEncode && testCAEncode

bytesCreateAccount :: BL.ByteString
bytesCreateAccount = BL.pack "{\"digest\":{\"hash\":\"hashy\",\"key\":\"mykey\"},\"payload\":{\"account\":\"TSLA\"}}"

testCADecode :: Bool
testCADecode =  (decode bytesCreateAccount :: Maybe CreateAccountRequest) == (Just (CreateAccountRequest {_payload = AccountPayload {_account = "TSLA"}, _digest = Digest {_hash = "hashy", _key = "mykey"}}))

testCADecode' :: Bool
testCADecode' = case (decode bytesCreateAccount :: Maybe CreateAccountRequest) of
                  Just (CreateAccountRequest (AccountPayload _) _) -> True
                  Nothing -> False

testCAEncode :: Bool
testCAEncode = (encode (CreateAccountRequest (AccountPayload (T.pack "TSLA")) (Digest (T.pack "hashy") (T.pack "mykey")))) == ("{\"payload\":{\"account\":\"TSLA\"},\"digest\":{\"hash\":\"hashy\",\"key\":\"mykey\"}}")
