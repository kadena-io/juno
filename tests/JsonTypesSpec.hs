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

testJsonCreateAccount :: Spec
testJsonCreateAccount = do
  it "decoding proper bytestring for CreateAccountRequest" $
    (decode bytesCreateAccount' :: Maybe CreateAccountRequest)
     `shouldBe`
      (Just (CreateAccountRequest
             {_payload = AccountPayload {_account = "TSLA"},
              _digest = Digest {_hash = "hashy", _key = "mykey"}
             })
      )
  it "encoding create account bytestring" $
     (encode (CreateAccountRequest (AccountPayload (T.pack "TSLA")) (Digest (T.pack "hashy") (T.pack "mykey"))))
     `shouldBe`
       ("{\"payload\":{\"account\":\"TSLA\"},\"digest\":{\"hash\":\"hashy\",\"key\":\"mykey\"}}")

testJsonAdjustAccount :: Spec
testJsonAdjustAccount = do
  it "decoding proper bytestring for AdjustAccountRequest" $
    (decode $ bytesAdjustRequestPayload :: Maybe AccountAdjustRequest)
     `shouldBe`
       Just (
             AccountAdjustRequest {
               _adjustAccountPayload = AccountAdjustPayload
               { _adjustAccount = "TSLA", _adjustAmount = 100.0}
             , _adjustAccountDigest = Digest {_hash = "hashy", _key = "mykey"}
             }
            )
  it "encoding adjust account bytestring" $
     (encode $ AccountAdjustRequest
                (AccountAdjustPayload (T.pack "TSLA") 100)
                (Digest (T.pack "hashy") (T.pack "mykey")))
     `shouldBe` bytesAdjustRequestPayload'


bytesCreateAccount' :: BL.ByteString
bytesCreateAccount' = BL.pack "{\"digest\":{\"hash\":\"hashy\",\"key\":\"mykey\"},\"payload\":{\"account\":\"TSLA\"}}"

bytesAdjustRequestPayload' :: BL.ByteString
bytesAdjustRequestPayload' =  BL.pack "{\"payload\":{\"amount\":100,\"account\":\"TSLA\"},\"digest\":{\"hash\":\"hashy\",\"key\":\"mykey\"}}"
