{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Apps.Juno.ApiDemoHandler (transferDemoReqHandler) where

import Control.Monad.Reader
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
    toTx to' from' amount = "transfer(" ++ to' ++ "->" ++ from' ++ "," ++ show (toRational amount) ++ ")"

-- demo API transfer
--{"payload":
--  {"transfer":
--   {"acctFrom":"000"
--   ,"acctTo":"001"
--   ,"amount":1192.00
--   ,"currency":"USD"
--   }
--   ,"data":
--   {"mtmessage":
--    "{1:F01CHASUS33AXXX0000456150}{2:O1032031160113CHASJPJTAXXX89653582351601132031N}{4:\r
--      \n:20:2016012800266559\r\n:23B:CRED\r\n:32A:160129USD24192,00\r\n:33B:USD24192,00\r
--      \n:50K:/6718000111\r\nSTANDARD LIFE INVESTMENTS LIMITED\r\n1 GEORGE STREET, \r
--      \nEDINBURGH SC EH2 2LL\r\nUNITED KINGDOM\r\n:52A:CHASGB2L\r\n:54A:CHASUS33\r\n:57A:CHASJPJT\r
--      \n:59:/6610024417\r\nTAKAO MARANA \r\nMARUNOUCHI,CHIYODAKU,\r\nTOKYO JAPAN \r
--      \n:70:/RFB/6064028LBA790001\r\n:71A:OUR\r\n-}\r\n\r\n"}
--   }
--}
--
data TransferJson = TransferJson { acctFromTrans :: String
                                 , acctToTrans :: String
                                 , amountTrans :: Double
                                 , currencyTrans :: String
                                 } deriving (Show, Eq)

instance ToJSON TransferJson where
  toJSON (TransferJson acctFrom  acctTo amount currency) =
      object [ "acctFrom" .= acctFrom
             , "acctTo" .= acctTo
             , "amount" .= amount
             , "currency" .= currency
             ]

instance FromJSON TransferJson where
  parseJSON (Object v) = TransferJson <$> v .: "acctFrom"
                                      <*> v .: "acctTo"
                                      <*> v .: "amount"
                                      <*> v .: "currency"

  parseJSON _ = mzero

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
    parseJSON (Object v) = TransactDemoRequest <$> ((v .: "payload") >>= (.: "transfer"))
                                               <*> ((v .: "payload") >>= (.: "data"))
    parseJSON _ = mzero

goodTrans = JSON.encode $ TransferJson "000" "001" 24192.10 "USD"
goodBody = JSON.encode $ TransferDataJson  "hi"

goodRequest = JSON.encode $ TransactDemoRequest
              (TransferJson "000" "001" 1192.00 "USD")
              (TransferDataJson "test blob")

decodeTransDemoReq = JSON.decode $ goodRequest :: Maybe TransactDemoRequest
