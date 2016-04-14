{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Apps.Juno.Ledger (
  dirtyPickOutAccount50a
  ,runQuery
  ,convertQuery
  ,Transaction(..)
  ,SwiftAPI(..)
  ,AcctRole(..)
  ,LedgerQuery(..)
  ,QueryResult(..)
  ) where

import Data.Either ()
import Control.Lens
import Control.Applicative ((<|>))
import Data.Aeson
import GHC.Natural
import Data.Text (Text, intercalate)

import qualified Data.Text as Text
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics
import Data.Map.Strict (Map)
import Data.Ratio
import qualified Data.Map.Strict as Map


import Juno.Hoplite.Eval (TransactionId(..), OpId(..), OrderedOp, Cmd(..))
import Schwifty.Swift.M105.Types


--newtype JunoEnv = JunoEnv {getStateMVar :: MV.MVar (DEval.PersistentState, Map.Map DEval.TransactionId SWIFT)}
--
--data Cmd = Cmd {from :: Text , to :: Text , posAmount :: Rational , fakeCryptoSig:: Text }
--  deriving (Eq,Ord,Show,Read,Data,Typeable)
--
--newtype TransactionId = TransactionId Natural
--  deriving (Eq, Show, Num, Enum, Ord)
--
--newtype OpId = OpId Natural
--  deriving (Eq, Show, Num, Enum, Ord)
--
--type OrderedOp = (OpId, Cmd)
--
--data PersistentState
--  = PersistentState { _persistentNextTxId :: TransactionId
--                    , _persistentBalances :: Map.Map Text Rational
--                    , _persistentTxes :: [(TransactionId, [OrderedOp])]
--                    }
--  deriving (Eq, Show)

data Transaction = Transaction {
   transId :: Int
  ,opId :: Int
  ,from :: Text
  ,to   :: Text
  ,amount :: Double
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data SwiftAPI = SwiftAPI {
   ref :: Text
  ,opCode :: Text
  ,orderingAcct :: Text
  ,orderingAcctDescription :: Text
  ,beneficiaryAcct :: Text
  ,beneficiaryAcctDescription :: Text
  ,settled :: Double
  ,currency :: Text
  ,valueDate :: Text
  ,details :: Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AcctRole = Sender | Receiver | Both deriving (Show, Eq, Generic, ToJSON, FromJSON)

data LedgerQuery = BySwiftId Integer
                 | ByAcctName AcctRole Text
                 | And [LedgerQuery]
                 -- | Or [LedgerQuery] -- If we need this, we'll add it
                 deriving (Show, Eq, Generic, ToJSON, FromJSON)

data QueryResult = QueryResult {
   swifts :: Map String SwiftAPI
  ,trans :: [Transaction]
  } deriving (Show, Eq, Generic, ToJSON)

-- these are here solely for convenience
type HopperLog = ([(TransactionId, [OrderedOp])], Map TransactionId SWIFT)
type TransLog = [(TransactionId, [OrderedOp])]
--type SwiftLog = Map TransactionId SWIFT

runQuery :: LedgerQuery -> HopperLog -> QueryResult
runQuery l hl = convertQuery $ execQuery l hl

execQuery :: LedgerQuery -> HopperLog -> HopperLog
execQuery (BySwiftId i) lss = filterById (fromInteger i) lss
execQuery (ByAcctName r i) lss = filterByAcct r i lss
execQuery (And []) _ = ([],Map.empty) -- if there's nothing to query, return nothing... techincally an error
execQuery (And [x]) lss = execQuery x lss
execQuery (And (x:xs)) lss = execQuery (And xs) (execQuery x lss)

filterById :: Natural -> HopperLog -> HopperLog
filterById i (l, ss) = (l', ss')
  where
    l' = filter (\(TransactionId i', _) -> i' == i) l
    ss' = Map.filterWithKey (\k _ -> k == TransactionId i) ss

filterByAcct :: AcctRole -> Text -> HopperLog -> HopperLog
filterByAcct r a (l, ss) = (l', ss')
  where
    l' = filter (acctInvolved r a . snd) l
    ss' = Map.filterWithKey (\k _ -> Set.member k (associatedSwiftIds l')) ss

acctInTrans :: AcctRole -> Text -> OrderedOp -> Bool
acctInTrans Sender a (_, Cmd to' _ _ _) = to' == a
acctInTrans Receiver a (_, Cmd _ from' _ _) = from' == a
acctInTrans Both a (_, Cmd to' from' _ _) = from' == a || to' == a

acctInvolved :: AcctRole -> Text -> [OrderedOp] -> Bool
acctInvolved r a = any (acctInTrans r a)

associatedSwiftIds :: TransLog -> Set TransactionId
associatedSwiftIds = Set.fromList . fmap fst

convertQuery :: HopperLog -> QueryResult
convertQuery (l, ss) = QueryResult ss' l'
  where
    ss' = Map.map convertSWIFT $ Map.mapKeys (\(TransactionId i) -> show i) ss
    l' = convertTrans l

convertSWIFT :: SWIFT -> SwiftAPI
convertSWIFT m = SwiftAPI
  (m ^. sCode20 . unSendersRef) -- ref :: Text
  (Text.pack $ show $ m ^. sCode23B) -- opCode :: Text
  (dirtyPickOutAccount50a m) -- orderingAcct :: Text
  (orderingAcctFreetext m) -- orderingAcctDescription :: Text
  (m ^. sCode59a . bcAccount) -- beneficiaryAcct :: Text
  (beneficiaryAcctFreetext m) -- beneficiaryAcctDescription :: Text
  (convertAmount m) -- settled :: Double
  (m ^. sCode32A . vcsCurrency) -- currency :: Text
  (Text.pack $ m ^. sCode32A . vcsValueDate . unTime ) -- valueDate :: Text
  (Text.pack $ show $ m ^. sCode71A) -- details :: Text

convertAmount :: SWIFT -> Double
convertAmount m = fromRational $ wholeDollars + cents
 where
   wholeDollars :: Rational
   wholeDollars = fromIntegral $ m ^. sCode32A . vcsSettlementAmount . vWhole
   stupidCents :: Ratio Int
   stupidCents =  m ^. sCode32A . vcsSettlementAmount . vPart
   cents :: Rational
   cents = (fromIntegral $ numerator stupidCents) % (fromIntegral $ denominator stupidCents)

dirtyPickOutAccount50a :: SWIFT -> Text
dirtyPickOutAccount50a s = case s ^? (sCode50a . ocA_Account . unAccount) of
  Just v -> v
  Nothing -> case s ^? (sCode50a . ocK_Account . unAccount) of
    Just v -> v
    Nothing -> case s ^? (sCode50a . ocF_Account . unF50F_Account . unAccount) of
      Just v -> v
      Nothing -> case s ^? (sCode50a . ocF_Account . unF50F_PartyId . piIdentifier) of
        Just v -> v
        Nothing -> error "Invariant Error: invalid swift detected, no Code50a account"

orderingAcctFreetext :: SWIFT -> Text
orderingAcctFreetext s = maybe "" (intercalate "\n") $
  (s ^? sCode50a . ocA_remainder . _Just)   <|>
    (s ^? sCode50a . ocF_remainder . _Just) <|>
    (s ^? sCode50a . ocK_remainder . _Just)

beneficiaryAcctFreetext :: SWIFT -> Text
beneficiaryAcctFreetext s = s ^. sCode59a . bcDetails

convertTrans :: TransLog -> [Transaction]
convertTrans t = concat $ convertEntry <$> t
  where
    convertOp :: TransactionId -> OrderedOp -> Transaction
    convertOp (TransactionId tId) (OpId oId, Cmd from' to' amt' _) =
      Transaction (fromIntegral tId) (fromIntegral oId) from' to' (fromRational amt')
    convertEntry :: (TransactionId, [OrderedOp]) -> [Transaction]
    convertEntry (tId, oOps) = convertOp tId <$> oOps
