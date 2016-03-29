{-# LANGUAGE OverloadedStrings #-}

module JsonTypesSpec where

import           Test.Hspec
import           Data.Aeson (encode
                            , decode
                            )
import qualified Data.ByteString.Lazy.Char8 as BL
import           Apps.Juno.JsonTypes
import qualified Data.Text as T


spec :: Spec
spec = do
  describe "JsonTypes Create Account for Juno REST API." testJsonCreateAccount
  describe "JsonTypes Adjust Account for Juno REST API." testJsonAdjustAccount
  describe "JsonTypes Poll Commands for Juno REST API." testJsonPoll

testJsonCreateAccount :: Spec
testJsonCreateAccount = do

  it "decoding CreateAccountRequest byteString" $
    (decode createAccountByteString :: Maybe CreateAccountRequest)
     `shouldBe`
      (Just (CreateAccountRequest
             {_payload = AccountPayload {_account = "TSLA"},
              _digest = Digest {_hash = "hashy", _key = "mykey"}
             })
      )

  it "encoding create account bytestring" $
     encodeCreateAccountRequest
     `shouldBe`
       ("{\"payload\":{\"account\":\"TSLA\"},\"digest\":{\"hash\":\"hashy\",\"key\":\"mykey\"}}")

testJsonAdjustAccount :: Spec
testJsonAdjustAccount = do

  it "decoding proper bytestring for AdjustAccountRequest" $
    decodeAdjustRequest
     `shouldBe`
       Just (
             AccountAdjustRequest {
               _adjustAccountPayload = AccountAdjustPayload
               { _adjustAccount = "TSLA", _adjustAmount = 100.0}
             , _adjustAccountDigest = Digest {_hash = "hashy", _key = "mykey"}
             }
            )

  it "encoding adjust account bytestring" $
     encodeAdjustRequest
     `shouldBe` bytesAdjustRequestPayload'

testJsonPoll :: Spec
testJsonPoll = do

  it "ecoding PollPayload" $
     encodePollRequest
     `shouldBe`
      pollRequestByteString

  it "encoding PollResult" $
     encodePollResult
     `shouldBe`
      (BL.pack "{\"status\":\"Accepted\",\"cmdid\":\"1\",\"payload\":\"res payload\",\"message\":\"nothing to say\",\"logidx\":-1}")

  it "decode pollPayloadReqeust" $
     decodePollPayloadRequest
     `shouldBe`
     Just (PollPayloadRequest {_pollPayload = PollPayload {_cmdids = ["1","2","3"]}, _pollDigest = Digest {_hash = "hashy", _key = "mykey"}})

  it "decode PollPayload only" $
     decodePollPayload
     `shouldBe`
     Just (PollPayload {_cmdids = ["1","2","3"]})

------------------------------------------------------------------------------------------------
--- Local Helper Functions
------------------------------------------------------------------------------------------------

-- Create Account helpers
digestByteString :: BL.ByteString
digestByteString = BL.pack "{\"hash\":\"hashy\",\"key\":\"mykey\"}"

decodeDigest :: Maybe Digest
decodeDigest = decode digestByteString :: Maybe Digest

encodeCreateAccountRequest :: BL.ByteString
encodeCreateAccountRequest = encode (CreateAccountRequest (AccountPayload (T.pack "TSLA")) (Digest (T.pack "hashy") (T.pack "mykey")))

createAccountByteString :: BL.ByteString
createAccountByteString = BL.pack "{\"digest\":{\"hash\":\"hashy\",\"key\":\"mykey\"},\"payload\":{\"account\":\"TSLA\"}}"
--
bytesAdjustRequestPayload' :: BL.ByteString
bytesAdjustRequestPayload' =  BL.pack "{\"payload\":{\"amount\":100,\"account\":\"TSLA\"},\"digest\":{\"hash\":\"hashy\",\"key\":\"mykey\"}}"


-- | Adjust account helpers

adjustPayloadDecode :: Bool
adjustPayloadDecode = (decode $ BL.pack "{\"amount\":100,\"account\":\"TSLA\"}" :: Maybe AccountAdjustPayload) == Just (AccountAdjustPayload {_adjustAccount = "TSLA", _adjustAmount = 100.0})

adjustPayloadEncode :: Bool
adjustPayloadEncode = (encode $ AccountAdjustPayload "TSLA" 100) == "{\"amount\":100,\"account\":\"TSLA\"}"

adjustRequestPayloadBS :: BL.ByteString
adjustRequestPayloadBS =  BL.pack "{\"payload\":{\"amount\":100,\"account\":\"TSLA\"},\"digest\":{\"hash\":\"hashy\",\"key\":\"mykey\"}}"

encodeAdjustRequest :: BL.ByteString
encodeAdjustRequest = encode $
                       AccountAdjustRequest (AccountAdjustPayload (T.pack "TSLA") 100)
                                           (Digest (T.pack "hashy") (T.pack "mykey"))

decodeAdjustRequest :: Maybe AccountAdjustRequest
decodeAdjustRequest = (decode $ adjustRequestPayloadBS :: Maybe AccountAdjustRequest)

-- | poll cmds helpers

encodePollRequest :: BL.ByteString
encodePollRequest = encode $ PollPayloadRequest
                        (PollPayload $ fmap T.pack ["1","2","3"])
                        (Digest {_hash = "hashy", _key = "mykey"})

pollRequestByteString :: BL.ByteString
pollRequestByteString = BL.pack $ "{\"payload\":{\"cmdids\":[\"1\",\"2\",\"3\"]},\"digest\":{\"hash\":\"hashy\",\"key\":\"mykey\"}}"

decodePollPayloadRequest :: Maybe PollPayloadRequest
decodePollPayloadRequest = (decode $ pollRequestByteString :: Maybe PollPayloadRequest)

decodedPollPayloadRequest :: Maybe PollPayloadRequest
decodedPollPayloadRequest = Just PollPayloadRequest {_pollPayload = PollPayload {_cmdids = ["1","2","3"]}, _pollDigest = Digest {_hash = "hashy", _key = "mykey"}}

decodePollPayload :: Maybe PollPayload
decodePollPayload = (decode $ pollPayloadByteString :: Maybe PollPayload)
                    where
                      pollPayloadByteString :: BL.ByteString
                      pollPayloadByteString = "{\"cmdids\":[\"1\",\"2\",\"3\"]}"

-- | Test PollResults
encodePollResult :: BL.ByteString
encodePollResult = encode $ PollResult {
                               _pollStatus = "Accepted"
                             , _pollCmdId = "1"
                             , _logidx = (-1)
                             , _pollMessage = "nothing to say"
                             , _pollResPayload = "res payload"
                            }
