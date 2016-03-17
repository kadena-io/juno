{-# LANGUAGE OverloadedStrings #-}

module Schwifty.Swift.M105.Parser (
  parseSwift
  ) where

import Data.Text (Text)
import Data.Ratio
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Text
import Control.Monad
import Control.Lens

import Schwifty.Swift.M105.Types


-- Field Parsers

senderRef :: Parser SendersRef
senderRef = do
  s <- count' 1 16 alphaNumChar <?> "[1,16] Char"
  return $ SendersRef $ T.pack s

timeIndication :: Parser TimeIndication
timeIndication = do
  _ <- char '/'
  c <- count' 1 8 letterChar
  _ <- char '/'
  t <- count 4 numberChar
  s <- char '-' <|> char '+'
  o <- count 4 numberChar
  return $ TimeIndication (T.pack c) (T.pack t) (T.pack [s]) (T.pack o)

bankOperationCode :: Parser BankOperationCode
bankOperationCode =  try (string "CRED" >> return NormalCreditTransfer)
                 <|> try (string "CRTS" >> return TestMessage)
                 <|> try (string "SPAY" >> return SWIFTPay)
                 <|> try (string "SPRI" >> return Priority)
                 <|> try (string "SSTD" >> return Standard)


instructionCode :: Parser InstructionCode
instructionCode =
  try (string "CORT" >> optional (char '/') >> optional (manyTill anyChar $ try newline)
           >>= return . CorporateTrade . fmap T.pack)
  <|> try (string "INTC"  >> optional (char '/') >> optional (manyTill anyChar $ try newline)
           >>= return . IntraCompanyPayment . fmap T.pack)
  <|> try (string "REPA"  >> optional (char '/') >> optional (manyTill anyChar $ try newline)
           >>= return . RelatedPayment . fmap T.pack)
  <|> try (string "SDVA"  >> optional (char '/') >> optional (manyTill anyChar $ try newline)
           >>= return . SameDayValue . fmap T.pack)

transactionTypeCode :: Parser TransactionTypeCode
transactionTypeCode = liftM (TransactionTypeCode . T.pack) $ count 3 alphaNumChar

valueParser :: Parser Value
valueParser = do
  amtWhole <- some numberChar
  _ <- char ','
  amtPart <- some numberChar
  return $ Value (read amtWhole :: Int)
                 ((read amtPart :: Int) % (10 ^ length amtPart))

currencyParser :: Parser Text
currencyParser = liftM T.pack (count 3 alphaNumChar <?> "Currency as ISO 4217 (e.g. XXX)")

vDateCurSetl :: Parser VDateCurSetl
vDateCurSetl = do
  dt <- count 6 numberChar <?> "Date in YYMMDD format"
  cur <- currencyParser
  val <- valueParser
  return $ VDateCurSetl (Time dt) cur val

currencyInstructedAmt :: Parser CurrencyInstructedAmt
currencyInstructedAmt = do
  cur <- currencyParser
  val <- valueParser
  return $ CurrencyInstructedAmt cur val

exchangeRate :: Parser ExchangeRate
exchangeRate = liftM ExchangeRate valueParser

idType :: Parser IdType
idType = try (string "ARNU" >> return AlienReg)
      <|> try (string "CCPT" >> return Passport)
      <|> try (string "CUST" >> return CustomerId)
      <|> try (string "DRLC" >> return DriversLicence)
      <|> try (string "EMPL" >> return Employer)
      <|> try (string "NIDN" >> return NationalId)
      <|> try (string "SOSE" >> return SocialSecurity)
      <|> try (string "TXID" >> return TaxId)

partyId :: Parser PartyId
partyId = do
  code <- idType
  _ <- char '/'
  countryCode <- count 2 letterChar
  _ <- char '/'
  identifier <- someTill anyChar newline
  return $ PartyId code (T.pack countryCode) (T.pack identifier)

account :: Parser Account
account = do
  _ <- char '/'
  acct <- someTill anyChar newline
  return $ Account $ T.pack acct

identifierCode :: Parser IdentifierCode
identifierCode = liftM (IdentifierCode . T.pack) $ space >> manyTill anyChar newline

field50FIdentifier :: Parser Field50FIdentifier
field50FIdentifier = try (liftM F50F_Account account)
                  <|> liftM F50F_PartyId partyId

field50FExtra :: Parser Field50FExtra
field50FExtra = try (space >> string "1/" >> manyTill anyChar newline >>= return . F50F_1_NameOrdCustomer . T.pack)
             <|> try (space >> string "2/" >> manyTill anyChar newline >>= return . F50F_2_AddressLine . T.pack)
             <|> try (space >> string "3/" >> manyTill anyChar newline >>= return . F50F_3_CountryAndTown . T.pack)
             <|> try (space >> string "4/" >> manyTill anyChar newline >>= return . F50F_4_DateOfBirth . T.pack)
             <|> try (space >> string "5/" >> manyTill anyChar newline >>= return . F50F_5_PlaceOfBirth . T.pack)
             <|> try (space >> string "6/" >> manyTill anyChar newline >>= return . F50F_6_CustIdNum . T.pack)
             <|> try (space >> string "7/" >> manyTill anyChar newline >>= return . F50F_7_NationalIdNum . T.pack)
             <|> try (space >> string "8/" >> manyTill anyChar newline >>= return . F50F_8_AdditionalInfo . T.pack)

field50FExtras :: Parser [Field50FExtra]
field50FExtras = manyTill field50FExtra eof

invalidCustomerExtra :: Parser Text
invalidCustomerExtra = liftM T.pack $ space >> manyTill anyChar newline

invalidCustomerExtras :: Parser (Maybe [Text])
invalidCustomerExtras = optional $ someTill invalidCustomerExtra eof

orderingCustomerA :: Parser OrderingCustomer
orderingCustomerA = do
  acct <- account
  ident <- identifierCode
  leftovers <- invalidCustomerExtras
  return $ Field50A acct ident leftovers

orderingCustomerF :: Parser OrderingCustomer
orderingCustomerF = do
  ident <- field50FIdentifier
  x <- optional field50FExtras
  leftovers <- invalidCustomerExtras
  return $ Field50F ident x leftovers

orderingCustomerK :: Parser OrderingCustomer
orderingCustomerK = do
  acct <- account
  x <- optional field50FExtras
  leftovers <- invalidCustomerExtras
  return $ Field50K acct x leftovers

beneficiaryCustomer :: Parser BeneficiaryCustomer
beneficiaryCustomer = do
  acct <- char '/' >> manyTill anyChar newline
  dets <- maybe "" (T.intercalate "\n") <$> invalidCustomerExtras
  return $ BeneficiaryCustomer (T.pack acct) dets

detailsOfCharges :: Parser DetailsOfCharges
detailsOfCharges = try (string "BEN" >> return Beneficiary)
                 <|> try (string "OUR" >> return OurCustomerCharged)
                 <|> try (string "SHA" >> return SharedCharges)

-- SWIFT Message Parser

parseCode :: Parser Code
parseCode = do
  _ <- char ':' <?> "SWIFT Tag Opening \":\""
  c <- count 3 anyChar
  _ <- char ':' <?> "SWIFT Tag Closing \":\""
  return $ Code $ T.pack c

parseBody :: Parser Text
parseBody = do
  b <- try (someTill anyChar (try $ lookAhead parseCode)) <|> someTill anyChar eof
  return $ T.pack b

parseToken :: Parser (Code, Text)
parseToken = do
  c <- parseCode
  b <- parseBody
  return (c, b)

tokenize :: Parser [(Code,Text)]
tokenize = manyTill anyChar (try $ lookAhead parseCode) >> many parseToken

validateSWIFT :: MaybeSWIFT -> Either String SWIFT
validateSWIFT msg = case reqs of
    [] -> Right finalSWIFT
    xs -> Left $ "Required Fields Not Found: " ++ show xs
  where
    go s l = maybe [s] (const []) (view l msg)
    reqs :: [String]
    reqs = go "20 " msCode20
           ++  go "23B" msCode23B
           ++  go "32A" msCode32A
           ++  go "50a" msCode50a
           ++  go "59a" msCode59a
           ++  go "71A" msCode71A
    finalSWIFT = SWIFT
        (fromJust $ view msCode20 msg)
        (view msCode13C msg)
        (fromJust $ view msCode23B msg)
        (view msCode23E msg)
        (view msCode26T msg)
        (fromJust $ view msCode32A msg)
        (view msCode33B msg)
        (view msCode36 msg)
        (fromJust $ view msCode50a msg)
        (view msCode51A msg)
        (view msCode52A msg)
        (view msCode53a msg)
        (view msCode54A msg)
        (view msCode55A msg)
        (view msCode56A msg)
        (view msCode57A msg)
        (fromJust $ view msCode59a msg)
        (view msCode70 msg)
        (fromJust $ view msCode71A msg)
        (view msCode71F msg)
        (view msCode71G msg)
        (view msCode72 msg)
        (view msCode77B msg)

mapToMaybeSWIFT :: [(Code, Text)] -> ([String], MaybeSWIFT)
mapToMaybeSWIFT = foldl go mempty
  where
    go :: ([String], MaybeSWIFT) -> (Code, Text) -> ([String], MaybeSWIFT)
    go (errs, ms) (c, t) = let (err, ms') = parseRest c t in (errs ++ err, ms `mappend` ms')

parseSwift :: Text -> Either String SWIFT
parseSwift s = case runParser tokenize "Swift Message" s of
                 Left err -> Left $ show err ++ "\n ## found in ## \n" ++ show s
                 Right v -> let (errs,m) = mapToMaybeSWIFT v
                            in case validateSWIFT m of
                                Left err -> Left $ err ++ "\n ## With Errors ##\n" ++ show errs
                                Right mv -> Right mv

parseBodyComment :: Parser String
parseBodyComment = manyTill anyChar (char ':') -- you want this ':' to capture as it delimits the end of a comma

restHelper :: (Monoid s) => Parsec Text b -> String -> Text -> ASetter' s (Maybe b) -> ([String], s)
restHelper p n t c = either (\x -> ([show x], mempty)) (\x -> ([], set c (Just x) mempty)) $ runParser (optional parseBodyComment >> p) n t

parseRest :: Code -> Text -> ([String], MaybeSWIFT)
parseRest (Code "20 ") t = restHelper senderRef "senderRef" t msCode20
parseRest (Code "13C") t = restHelper timeIndication "timeIndication" t msCode13C
parseRest (Code "23B") t = restHelper bankOperationCode "timeIndication" t msCode23B
parseRest (Code "23E") t = restHelper instructionCode "instructionCode" t msCode23E
parseRest (Code "26T") t = restHelper transactionTypeCode "transactionTypeCode" t msCode26T
parseRest (Code "32A") t = restHelper vDateCurSetl "vDateCurSetl" t msCode32A
parseRest (Code "33B") t = restHelper currencyInstructedAmt "currencyInstructedAmt" t msCode33B
parseRest (Code "36 ") t = restHelper exchangeRate "exchangeRate" t msCode36
parseRest (Code "50A") t = restHelper orderingCustomerA "orderingCustomer" t msCode50a
parseRest (Code "50F") t = restHelper orderingCustomerF "orderingCustomer" t msCode50a
parseRest (Code "50K") t = restHelper orderingCustomerK "orderingCustomer" t msCode50a
parseRest (Code "51A") _t =  ([],mempty) -- restHelper undefined "undefined" t msCode51A
parseRest (Code "52A") _t =  ([],mempty) -- restHelper undefined "undefined" t msCode52A
parseRest (Code "53A") _t =  ([],mempty) -- restHelper undefined "undefined" t msCode53a
parseRest (Code "53B") _t =  ([],mempty) -- restHelper undefined "undefined" t msCode53a
parseRest (Code "54A") _t =  ([],mempty) -- restHelper undefined "undefined" t msCode54A
parseRest (Code "55A") _t =  ([],mempty) -- restHelper undefined "undefined" t msCode55A
parseRest (Code "56A") _t =  ([],mempty) -- restHelper undefined "undefined" t msCode56A
parseRest (Code "57A") _t =  ([],mempty) -- restHelper undefined "undefined" t msCode57A
parseRest (Code "59 ") t = restHelper beneficiaryCustomer "beneficiaryCustomer" t msCode59a
parseRest (Code "59a") t = restHelper beneficiaryCustomer "beneficiaryCustomer" t msCode59a
parseRest (Code "70 ") _t =  ([],mempty) -- restHelper undefined "undefined" t msCode70
parseRest (Code "71A") t = restHelper detailsOfCharges "detailsOfCharges" t msCode71A
parseRest (Code "71F") _t =  ([],mempty) -- restHelper undefined "undefined" t msCode71F
parseRest (Code "71G") _t =  ([],mempty) -- restHelper undefined "undefined" t msCode71G
parseRest (Code "72 ") _t =  ([],mempty) -- restHelper undefined "undefined" t msCode72
parseRest (Code "77B") _t =  ([],mempty) -- restHelper undefined "undefined" t msCode77B
parseRest (Code x) t = (["Unexepected Code: '" ++ show x ++ "' With Body: " ++ show t], mempty)
