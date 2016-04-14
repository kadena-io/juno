{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Juno.Hoplite.Transmatic
    ( -- * Parse
      Txm (..)
    , tInteger, tString, tAtom, tList
    , expr
      -- * Compile
    , compile
    , compileText
    , compileFile
    ) where
import Data.Ratio
import Data.Maybe
import Juno.Hoplite.Types hiding (Exp(..))
import Juno.Hoplite.Term
import Juno.Hoplite.Eval
import qualified Data.Map as Map
import qualified Text.Trifecta as TF
import Text.Parser.Char
import Text.Parser.Token
import Text.Parser.Combinators
import qualified Data.Text as T
import Data.Text (Text)
import Control.Applicative
import Control.Lens
import Control.Monad
import qualified Text.PrettyPrint.ANSI.Leijen as PP


--
-- PARSE
--

data Txm = TNumber { _tInteger :: Integer }
         | TString { _tString :: Text }
         | TAtom { _tAtom :: Text }
         | TList { _tList :: [Txm] }
         deriving (Eq,Show)

$(makeLenses ''Txm)

symbols :: CharParsing m => m Char
symbols = oneOf "#%+-_&$@"

style :: CharParsing m => IdentifierStyle m
style = IdentifierStyle "transmatic"
        (letter <|> symbols)
        (letter <|> digit <|> symbols)
        mempty
        minBound
        minBound

expr :: (Monad m,TokenParsing m,CharParsing m) => m Txm
expr = (TNumber <$> integer) <|>
       (TString <$> stringLiteral) <|>
       (TAtom <$> ident style) <|>
       (TList <$> parens (sepBy expr spaces))

parseText :: TF.Parser a -> Text -> TF.Result a
parseText p = TF.parseString p mempty . T.unpack

--
-- COMPILE
--

compile :: Txm -> Either String HopliteTerm
compile (TNumber n) = return $ ELit (LInteger n)
compile (TString s) = return $ ELit (LText s)
compile (TList l) =
    case l of
      [] -> Left "Empty list, c'est impossible"
      -- (let bindings forms)
      (TAtom a:TList bindings:forms) | a == "let" -> do
            bindings' <- forM bindings $ \b ->
                         case b of
                           (TList [TAtom n,v]) -> (n,) <$> compile v
                           bad -> Left $ "Invalid binding: " ++ show bad
            case bindings' of
              [] -> Left "Empty bindings in let"
              ((n1,v1):bs) -> do
                let lbs = foldr (\(n,v) e -> e . Let n v) (Let n1 v1) bs
                forms' <- mapM compile forms
                let lbsfs = foldr (\v e -> e . Let "__" v) lbs forms'
                return $ lbsfs (V "__")
      -- (% n d)
      [TAtom a,TNumber n,TNumber d] | a == "%" -> return (ELit (LRational (n % d)))
      -- (#prim a b ...)
      (TAtom a:args) | T.take 1 a == "#" && T.length a > 1 -> do
             as <- mapM compile args
             return (PrimApp (T.drop 1 a) as)
      -- (lambda args body)
      [TAtom a,TList args,body] | a == "lambda" -> do
             atoms <- case sequence . map (firstOf tAtom) $ args of
                 Nothing -> Left $ "Non-atom in lambda args: " ++ show args
                 Just v -> return v
             body' <- compile body
             return (Lam atoms body')
      -- (atom a b ...)
      (TAtom a:args) -> do
             args' <- mapM compile args
             return $ (V a :@ args')
      _ -> Left $ "Unrecognized list form: " ++ show l
compile (TAtom a) = return (V a)

compileText :: Text -> Either String HopliteTerm
compileText s = case parseText expr s of
  TF.Failure _ -> Left "Parse failed"
  TF.Success t -> compile t

compileFile :: FilePath -> IO (Either String HopliteTerm)
compileFile f = do
  p <- TF.parseFromFileEx expr f
  case p of
    (TF.Failure x) -> PP.putDoc x >> return (Left "Parse failed")
    (TF.Success t) -> return $ compile t


--
-- GHCI utils
--

_runProgram :: PersistentState -> HopliteTerm -> InterpreterOutput
_runProgram state p = case runExpr 10000 state ex of
  Left e -> error $ "fail: " ++ (show e)
  Right output -> output
  where
    (PolyF ex) = fromJust $ evaluableHopliteTerm p

_accountsCreated :: PersistentState
_accountsCreated = initialState { _persistentBalances = Map.fromList
                                 [ ("a", 1000 % 1)
                                 , ("b", 1000 % 1)
                                 , ("00689601574", 1000 % 1)
                                 , ("jpm", 1000 % 1)
                                 , ("00273601", 1000 % 1)] }

_run :: HopliteTerm -> InterpreterOutput
_run = _runProgram _accountsCreated
