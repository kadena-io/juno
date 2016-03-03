{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Apps.Juno.JsonModels where

import Data.Aeson (encode, decode, ToJSON, FromJSON)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import GHC.Generics
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)

data Digest = Digest { hash :: String, key :: String } deriving (Eq, Generic, Show)
instance ToJSON Digest where
instance FromJSON Digest where

-- | CreateAccountRequest
-- encode (CreateAccoutRequest (AccountPayload "TSLA") (Digest "hashy" "mykey"))
-- "{\"_digest\":{\"_hash\":\"hashy\",\"_key\":\"mykey\"},\"_payload\":{\"_account\":\"TSLA\"}}"
-- decode bytesCreateAccount :: Maybe CreateAccountRequest
data AccountPayload = AccountPayload { account :: String } deriving (Eq, Generic, Show)
instance ToJSON AccountPayload where
instance FromJSON AccountPayload where

data CreateAccountRequest = CreateAccountRequest {
      payload :: AccountPayload,
      digest :: Digest
    } deriving (Eq, Generic, Show)
instance ToJSON CreateAccountRequest where
instance FromJSON CreateAccountRequest where

data CreateAccountResponse = CreateAccountResponse {
      cmdid :: String
    , status :: String
    } deriving (Eq, Generic, Show)
instance ToJSON CreateAccountResponse where
instance FromJSON CreateAccountResponse where

createAccountResponseSuccess :: String -> CreateAccountResponse
createAccountResponseSuccess cid = CreateAccountResponse cid "Success"

createAccountResponseFailure :: String -> CreateAccountResponse
createAccountResponseFailure cid = CreateAccountResponse cid "Failure"


-- Tests
test :: Bool
test = testCAEncode && testCAEncode

bytesCreateAccount = (encodeUtf8 . TL.pack) "{\"digest\":{\"hash\":\"hashy\",\"key\":\"mykey\"},\"payload\":{\"account\":\"TSLA\"}}"

testCADecode :: Bool
testCADecode =  (decode bytesCreateAccount :: Maybe CreateAccountRequest) == (Just (CreateAccountRequest {payload = AccountPayload {account = "TSLA"}, digest = Digest {hash = "hashy", key = "mykey"}}))

testCADecode' :: Bool
testCADecode' = case (decode bytesCreateAccount :: Maybe CreateAccountRequest) of
                  Just (CreateAccountRequest (AccountPayload account) digest) -> True
                  Nothing -> False

testCAEncode :: Bool
testCAEncode = (encode (CreateAccountRequest (AccountPayload "TSLA") (Digest "hashy" "mykey"))) == ("{\"digest\":{\"hash\":\"hashy\",\"key\":\"mykey\"},\"payload\":{\"account\":\"TSLA\"}}")
