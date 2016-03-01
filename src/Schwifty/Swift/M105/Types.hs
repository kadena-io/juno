{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Schwifty.Swift.M105.Types where

import Data.Text (Text)


import Control.Applicative
import Data.Ratio
import Control.Lens

newtype Code = Code {_unCode :: Text} deriving (Eq, Show, Ord)
$(makeLenses ''Code)

newtype Time = Time {_unTime :: String } deriving (Read,Show,Eq)
$(makeLenses ''Time)

data Value = Value {
  _vWhole :: !Int
  ,_vPart :: !(Ratio Int)
  } deriving (Read,Show,Eq)
$(makeLenses ''Value)

newtype SendersRef = SendersRef { _unSendersRef :: Text } deriving (Show,Eq)
$(makeLenses ''SendersRef)

data TimeIndication = TimeIndication {
  _tiCode :: !Text
  ,_tiTime :: !Text
  ,_tiSign :: !Text
  ,_tiOffset :: !Text
  } deriving (Show,Eq)
$(makeLenses ''TimeIndication)

data BankOperationCode = NormalCreditTransfer
                       | TestMessage
                       | SWIFTPay
                       | Priority
                       | Standard
                       deriving (Show,Eq)

data InstructionCode = CorporateTrade (Maybe Text)
                     | IntraCompanyPayment (Maybe Text)
                     | RelatedPayment (Maybe Text)
                     | SameDayValue (Maybe Text)
  deriving (Show,Eq)

newtype TransactionTypeCode = TransactionTypeCode {
  _unTransactionTypeCode :: Text } deriving (Show,Eq)
$(makeLenses ''TransactionTypeCode)

data VDateCurSetl = VDateCurSetl {
  _vcsValueDate :: !Time
  ,_vcsCurrency :: !Text
  ,_vcsSettlementAmount :: !Value
  } deriving (Show,Eq)
$(makeLenses ''VDateCurSetl)

data CurrencyInstructedAmt = CurrencyInstructedAmt {
  _ciaCurrency :: !Text
  ,_ciaAmount :: !Value
  } deriving (Show,Eq)
$(makeLenses ''CurrencyInstructedAmt)

newtype SettleAmount = SettleAmount {
  _unSettleAmount :: Double } deriving (Show,Eq)
$(makeLenses ''SettleAmount)

newtype ExchangeRate = ExchangeRate {
  _unExchangeRate :: Value } deriving (Show,Eq)
$(makeLenses ''ExchangeRate)


newtype SendingInstitution = SendingInstitution {
  _unSendingInstitution :: Text } deriving (Show,Eq)
$(makeLenses ''SendingInstitution)

newtype Account = Account {
  _unAccount :: Text} deriving (Show, Eq)
$(makeLenses ''Account)

newtype IdentifierCode = IdentifierCode {
  _unIdentifierCode :: Text} deriving (Show,Eq)
$(makeLenses ''IdentifierCode)

data IdType = AlienReg
            | Passport
            | CustomerId
            | DriversLicence
            | Employer
            | NationalId
            | SocialSecurity
            | TaxId
            deriving (Show, Eq, Enum)

data PartyId = PartyId {
  _piCode :: !IdType
  ,_piCountry :: !Text
  ,_piIdentifier :: !Text
  } deriving (Show, Eq)
$(makeLenses ''PartyId)

data Field50FExtra = F50F_1_NameOrdCustomer {_f50FNameOrdCust :: !Text}
                   | F50F_2_AddressLine {_f50FAddressLine :: !Text}
                   | F50F_3_CountryAndTown {_f50FCountryAndTown :: !Text}
                   | F50F_4_DateOfBirth {_f50FDateOfBirth :: !Text}
                   | F50F_5_PlaceOfBirth {_f50FPlaceOfBirth :: !Text}
                   | F50F_6_CustIdNum {_f50FCustIdNum :: !Text}
                   | F50F_7_NationalIdNum {_f50FNationalIdNum :: !Text}
                   | F50F_8_AdditionalInfo {_f50FAdditionalInfo :: !Text}
                   deriving (Show, Eq)
$(makeLenses ''Field50FExtra)

data Field50FIdentifier = F50F_PartyId { _unF50F_PartyId :: !PartyId }
                        | F50F_Account { _unF50F_Account :: !Account}
                        deriving (Show, Eq)
$(makeLenses ''Field50FIdentifier)

data OrderingCustomer
  = Field50A { _ocA_Account :: !Account
             , _ocA_IdCode :: !IdentifierCode
             , _ocA_remainder :: !(Maybe [Text]) }
  | Field50F { _ocF_Account :: !Field50FIdentifier
             , _ocF_extra :: !(Maybe [Field50FExtra])
             , _ocF_remainder :: !(Maybe [Text]) }
  | Field50K { _ocK_Account :: !Account
             , _ocK_extra :: !(Maybe [Field50FExtra])
             , _ocK_remainder :: !(Maybe [Text]) }
  deriving (Show,Eq)
$(makeLenses ''OrderingCustomer)

--data OrderingCustomer = OrderingCustomer {
--  _ocAccount :: !Text -- this is wrong and should distinguish between acct and id
--  ,_ocDetails :: !Text
--  } deriving (Show,Eq)

newtype OrderingInstitution = OrderingInstitution {
  _unOrderingInstitution :: Text } deriving (Show,Eq)
$(makeLenses ''OrderingInstitution)

newtype SendersCorrespondent = SendersCorrespondent {
  _unSendersCorrespondent :: Text } deriving (Show,Eq)
$(makeLenses ''SendersCorrespondent)

newtype ReceiversCorrespondent = ReceiversCorrespondent {
  _unReceiversCorrespondent :: Text } deriving (Show,Eq)
$(makeLenses ''ReceiversCorrespondent)

newtype ThirdReimbursementInstitution = ThirdReimbursementInstitution {
  _unThirdReimbursementInstitution :: Text } deriving (Show,Eq)
$(makeLenses ''ThirdReimbursementInstitution)

newtype IntermediaryInstitution = IntermediaryInstitution {
  _unIntermediaryInstitution :: Text } deriving (Show,Eq)
$(makeLenses ''IntermediaryInstitution)

newtype AccountWithInstitution = AccountWithInstitution {
  _unAccountWithInstitution :: Text } deriving (Show,Eq)
$(makeLenses ''AccountWithInstitution)

data BeneficiaryCustomer = BeneficiaryCustomer {
  _bcAccount :: !Text -- this is wrong and should distinguish between acct and id
  ,_bcDetails :: !Text
  } deriving (Show,Eq)
$(makeLenses ''BeneficiaryCustomer)

newtype RemittanceInformation = RemittanceInformation {
  _unRemittanceInformation :: Text } deriving (Show,Eq)
$(makeLenses ''RemittanceInformation)

data DetailsOfCharges = Beneficiary
                      | OurCustomerCharged
                      | SharedCharges
                      deriving (Show,Eq)

newtype SendersCharges = SendersCharges {
  _unSendersCharges :: Text } deriving (Show,Eq)
$(makeLenses ''SendersCharges)

newtype ReceiversCharges = ReceiversCharges {
  _unReceiversCharges :: Text } deriving (Show,Eq)
$(makeLenses ''ReceiversCharges)

newtype SenderToReceiverInfo = SenderToReceiverInfo {
  _unSenderToReceiverInfo :: Text } deriving (Show,Eq)
$(makeLenses ''SenderToReceiverInfo)

newtype RegulatoryReporting = RegulatoryReporting {
  _unRegulatoryReporting :: Text } deriving (Show,Eq)
$(makeLenses ''RegulatoryReporting)

data SWIFT = SWIFT {
  _sCode20   :: !SendersRef
  ,_sCode13C :: !(Maybe TimeIndication)
  ,_sCode23B :: !BankOperationCode
  ,_sCode23E :: !(Maybe InstructionCode)
  ,_sCode26T :: !(Maybe TransactionTypeCode)
  ,_sCode32A :: !VDateCurSetl
  ,_sCode33B :: !(Maybe CurrencyInstructedAmt)
  ,_sCode36  :: !(Maybe ExchangeRate)
  ,_sCode50a :: !OrderingCustomer
  ,_sCode51A :: !(Maybe SendingInstitution)
  ,_sCode52A :: !(Maybe OrderingInstitution)
  ,_sCode53a :: !(Maybe SendersCorrespondent)
  ,_sCode54A :: !(Maybe ReceiversCorrespondent)
  ,_sCode55A :: !(Maybe ThirdReimbursementInstitution)
  ,_sCode56A :: !(Maybe IntermediaryInstitution)
  ,_sCode57A :: !(Maybe AccountWithInstitution)
  ,_sCode59a :: !BeneficiaryCustomer
  ,_sCode70  :: !(Maybe RemittanceInformation)
  ,_sCode71A :: !DetailsOfCharges
  ,_sCode71F :: !(Maybe SendersCharges)
  ,_sCode71G :: !(Maybe ReceiversCharges)
  ,_sCode72  :: !(Maybe SenderToReceiverInfo)
  ,_sCode77B :: !(Maybe RegulatoryReporting)
  } deriving (Show, Eq)

$(makeLenses ''SWIFT)

data MaybeSWIFT = MaybeSWIFT {
  _msCode20   :: !(Maybe SendersRef)
  ,_msCode13C :: !(Maybe TimeIndication)
  ,_msCode23B :: !(Maybe BankOperationCode)
  ,_msCode23E :: !(Maybe InstructionCode)
  ,_msCode26T :: !(Maybe TransactionTypeCode)
  ,_msCode32A :: !(Maybe VDateCurSetl)
  ,_msCode33B :: !(Maybe CurrencyInstructedAmt)
  ,_msCode36  :: !(Maybe ExchangeRate)
  ,_msCode50a :: !(Maybe OrderingCustomer)
  ,_msCode51A :: !(Maybe SendingInstitution)
  ,_msCode52A :: !(Maybe OrderingInstitution)
  ,_msCode53a :: !(Maybe SendersCorrespondent)
  ,_msCode54A :: !(Maybe ReceiversCorrespondent)
  ,_msCode55A :: !(Maybe ThirdReimbursementInstitution)
  ,_msCode56A :: !(Maybe IntermediaryInstitution)
  ,_msCode57A :: !(Maybe AccountWithInstitution)
  ,_msCode59a :: !(Maybe BeneficiaryCustomer)
  ,_msCode70  :: !(Maybe RemittanceInformation)
  ,_msCode71A :: !(Maybe DetailsOfCharges)
  ,_msCode71F :: !(Maybe SendersCharges)
  ,_msCode71G :: !(Maybe ReceiversCharges)
  ,_msCode72  :: !(Maybe SenderToReceiverInfo)
  ,_msCode77B :: !(Maybe RegulatoryReporting)
  } deriving (Show, Eq)

$(makeLenses ''MaybeSWIFT)

instance Monoid MaybeSWIFT where
  mempty = MaybeSWIFT {
     _msCode20  = Nothing
    ,_msCode13C = Nothing
    ,_msCode23B = Nothing
    ,_msCode23E = Nothing
    ,_msCode26T = Nothing
    ,_msCode32A = Nothing
    ,_msCode33B = Nothing
    ,_msCode36  = Nothing
    ,_msCode50a = Nothing
    ,_msCode51A = Nothing
    ,_msCode52A = Nothing
    ,_msCode53a = Nothing
    ,_msCode54A = Nothing
    ,_msCode55A = Nothing
    ,_msCode56A = Nothing
    ,_msCode57A = Nothing
    ,_msCode59a = Nothing
    ,_msCode70  = Nothing
    ,_msCode71A = Nothing
    ,_msCode71F = Nothing
    ,_msCode71G = Nothing
    ,_msCode72  = Nothing
    ,_msCode77B = Nothing }
  (MaybeSWIFT
        msCode20'
        msCode13C'
        msCode23B'
        msCode23E'
        msCode26T'
        msCode32A'
        msCode33B'
        msCode36'
        msCode50a'
        msCode51A'
        msCode52A'
        msCode53a'
        msCode54A'
        msCode55A'
        msCode56A'
        msCode57A'
        msCode59a'
        msCode70'
        msCode71A'
        msCode71F'
        msCode71G'
        msCode72'
        msCode77B'
    ) `mappend` (
    MaybeSWIFT
        msCode20''
        msCode13C''
        msCode23B''
        msCode23E''
        msCode26T''
        msCode32A''
        msCode33B''
        msCode36''
        msCode50a''
        msCode51A''
        msCode52A''
        msCode53a''
        msCode54A''
        msCode55A''
        msCode56A''
        msCode57A''
        msCode59a''
        msCode70''
        msCode71A''
        msCode71F''
        msCode71G''
        msCode72''
        msCode77B'') = MaybeSWIFT {
             _msCode20 = msCode20' <|> msCode20''
            ,_msCode13C = msCode13C' <|> msCode13C''
            ,_msCode23B = msCode23B' <|> msCode23B''
            ,_msCode23E = msCode23E' <|> msCode23E''
            ,_msCode26T = msCode26T' <|> msCode26T''
            ,_msCode32A = msCode32A' <|> msCode32A''
            ,_msCode33B = msCode33B' <|> msCode33B''
            ,_msCode36 = msCode36' <|> msCode36''
            ,_msCode50a = msCode50a' <|> msCode50a''
            ,_msCode51A = msCode51A' <|> msCode51A''
            ,_msCode52A = msCode52A' <|> msCode52A''
            ,_msCode53a = msCode53a' <|> msCode53a''
            ,_msCode54A = msCode54A' <|> msCode54A''
            ,_msCode55A = msCode55A' <|> msCode55A''
            ,_msCode56A = msCode56A' <|> msCode56A''
            ,_msCode57A = msCode57A' <|> msCode57A''
            ,_msCode59a = msCode59a' <|> msCode59a''
            ,_msCode70 = msCode70' <|> msCode70''
            ,_msCode71A = msCode71A' <|> msCode71A''
            ,_msCode71F = msCode71F' <|> msCode71F''
            ,_msCode71G = msCode71G' <|> msCode71G''
            ,_msCode72 = msCode72' <|> msCode72''
            ,_msCode77B = msCode77B' <|> msCode77B''
            }
