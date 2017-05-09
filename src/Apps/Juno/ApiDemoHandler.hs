{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Apps.Juno.ApiDemoHandler (transferDemoReqHandler) where

import Control.Monad.Reader
import Data.List (intercalate)
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Aeson as JSON
import           GHC.Generics
import           Data.Aeson as JSON
import Apps.Juno.Parser (programCodeDelimiter)
import Apps.Juno.JsonTypes
import Juno.Types hiding (CommandBatch)


transferDemoReqHandler :: BLC.ByteString -> Either BLC.ByteString [CommandEntry]
transferDemoReqHandler bs =
    case JSON.decode bs of
      Just (TransactDemoRequest
            (TransferJson to' from' amount' _) -- ignoring currency for now USD
            (TransferDataJson progData)
           ) -> do
          let txCode = toTx to' from' amount'
          Right [CommandEntry $ BSC.pack $ progData ++ programCodeDelimiter ++ txCode]
      Nothing ->
          Left $ JSON.encode $ commandResponseFailure ""
                   "Malformed input, could not decode input JSON."
  where
    intermediaries = ["100","101","102","103"]
    branchA2BranchB = intercalate "->" intermediaries
    toTx to' from' amount = "transfer(" ++ to' ++ "->" ++ branchA2BranchB ++ "->" ++ from' ++ "," ++ show (toRational amount) ++ ")"

{-
 demo API transfer
{"payload":
  {"transfer":
   {"acctFrom":"000"
   ,"acctTo":"001"
   ,"amount":1192.00
   ,"currency":"USD"
   }
   ,"data":
   {"mtmessage":
    ""}
   }
}
-}

data TransferJson = TransferJson { _tAcctFrom :: String
                                 , _tAcctTo :: String
                                 , _tAmount :: Double
                                 , _Tcurrency :: String
                                 } deriving (Show, Eq)

instance ToJSON TransferJson where
  toJSON (TransferJson acctFrom  acctTo amount currency) =
      object [ "acctFrom" .= acctFrom
             , "acctTo" .= acctTo
             , "amount" .= amount
             , "currency" .= currency
             ]

instance FromJSON TransferJson where
  parseJSON = withObject "Transfer" $ \obj ->
                TransferJson <$> obj .:"acctFrom"
                             <*> obj .: "acctTo"
                             <*> obj .: "amount"
                             <*> obj .: "currency"

data TransferDataJson = TransferDataJson String deriving (Show, Eq)

instance ToJSON TransferDataJson where
  toJSON (TransferDataJson databody) =
      object ["mtmessage" .= databody]

instance FromJSON TransferDataJson where
  parseJSON (Object v) = TransferDataJson <$> v .: "mtmessage"

  parseJSON _ = mzero

data TransactDemoRequest = TransactDemoRequest TransferJson TransferDataJson
                           deriving (Show, Eq, Generic)

instance ToJSON TransactDemoRequest where
    toJSON (TransactDemoRequest transfer' databody) =
        object [ "payload" .= object [ "transfer" .=  transfer'
                                       ,"data" .= databody
                                      ]
               ]

instance FromJSON TransactDemoRequest where

  parseJSON = withObject "TransactDemoRequest" $ \obj ->
                TransactDemoRequest <$> ((obj .: "payload") >>= (.: "transfer"))
                                    <*> ((obj .: "payload") >>= (.: "data"))

_goodTrans :: BLC.ByteString
_goodTrans = JSON.encode $ TransferJson "000" "001" 24192.10 "USD"

_goodBody :: BLC.ByteString
_goodBody = JSON.encode $ TransferDataJson "hi"

_goodRequest :: BLC.ByteString
_goodRequest = JSON.encode $ TransactDemoRequest
              (TransferJson "000" "001" 1192.00 "USD")
              (TransferDataJson "test blob")

_decodeTransDemoReq :: Maybe TransactDemoRequest
_decodeTransDemoReq = JSON.decode $ _goodRequest :: Maybe TransactDemoRequest
