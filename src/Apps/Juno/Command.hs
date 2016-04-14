{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Apps.Juno.Command where

import Data.Either ()
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Juno.Hoplite.Term as DTerm
import qualified Juno.Hoplite.Eval as DEval
import qualified Control.Concurrent.MVar as MV
import Control.Exception (SomeException, handle)
import Control.Lens hiding ((.=))


import Data.Aeson (encode, object, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)

import Juno.Runtime.Types (CommandEntry(..), CommandResult(..))

import Apps.Juno.Parser
import Apps.Juno.Ledger (runQuery, convertQuery)
import Schwifty.Swift.M105.Types (SWIFT)

-- state of hopper
newtype JunoEnv = JunoEnv {getStateMVar :: MV.MVar (DEval.PersistentState, Map.Map DEval.TransactionId SWIFT)}

-- hopper
starterEnv :: IO JunoEnv
starterEnv = JunoEnv <$> MV.newMVar (DEval.initialState,Map.empty)

balances :: DEval.PersistentState -> Map.Map Text Rational
balances (DEval.PersistentState _ bals _) = bals

setBalances :: Map.Map Text Rational -> DEval.PersistentState -> DEval.PersistentState
setBalances bals ps = ps { DEval._persistentBalances = bals }

--JunoEnv:  MVar (State, Map(TX -> Swift))
runCommand :: JunoEnv -> CommandEntry -> IO CommandResult
runCommand env cmd' = do
  mvar <- return $ getStateMVar env
  (ps, ss) <- MV.takeMVar mvar -- persistent s, swift
  let bals = balances ps
  case readHopper $ unCommandEntry cmd' of
    Left err -> do
      MV.putMVar mvar (ps,ss)
      return $ CommandResult $ BSC.pack err
    Right cmd -> fmap CommandResult $ handle
        (\e -> do
            MV.putMVar mvar (ps,ss)
            return $ BSC.pack $ show (e :: SomeException)) $
        case cmd of
            CreateAccount acct ->
                if Map.member acct bals
                then do
                    MV.putMVar mvar (ps,ss)
                    return "Account Already Exists"
                else do
                    MV.putMVar mvar $! (setBalances (Map.insert acct 0 bals) ps, ss)
                    return $ BSC.pack $ "Created Account: " ++ show acct

            AdjustAccount acct amount ->
                if Map.member acct bals
                then do
                    MV.putMVar mvar $! (setBalances (Map.adjust (+ amount) acct bals) ps, ss)
                    return $ BSC.pack $ "Adjusted Account " ++ show acct ++ " by " ++ show amount
                else do
                    MV.putMVar mvar (ps, ss)
                    return $ BSC.pack $ "Error: Account " ++ show acct ++ " does not exist!"

            ObserveAccount acct -> do
                MV.putMVar mvar (ps,ss)
                return $ BSC.pack $ show $ Map.lookup acct bals

            ObserveAccounts -> do
                MV.putMVar mvar (ps, ss)
                return $ BSC.pack $ prettyLedger bals

            Program term ->
                case DTerm.evaluableHopliteTerm term of
                    Nothing -> do
                      MV.putMVar mvar (ps, ss)
                      return "Invalid Command or Program"
                    Just (DTerm.PolyF evaluableTerm) ->
                        case DEval.runExpr 10000 ps evaluableTerm of
                            Left err -> do
                                MV.putMVar mvar (ps, ss)
                                return $ BSC.pack $ "Execution issue " ++ show err
                            Right (DEval.InterpreterOutput (DEval.InterpreterDiff _balsDiff ops)
                                                        nextPs) -> do
                                MV.putMVar mvar (nextPs, ss)
                                return $ BSC.pack $ "Success"
                                        ++ "\n## Transaction Log ##\n" ++ unlines (prettyOpLog . snd <$> ops)
                                        ++ "\n ## Previous Ledger ## " ++ prettyLedger bals
                                        ++ "\n## New Ledger ##" ++ prettyLedger (balances nextPs)

            SwiftPayment s term ->
                case DTerm.evaluableHopliteTerm term of
                    Nothing -> do
                      MV.putMVar mvar (ps, ss)
                      return "Invalid SwiftPayment Command"
                    Just (DTerm.PolyF evaluableTerm) ->
                        case DEval.runExpr 10000 ps evaluableTerm of
                            Left err -> do
                                MV.putMVar mvar (ps, ss)
                                return $ BLC.toStrict $ encode $
                                    object [ "status" .= ("Failure" :: T.Text)
                                           , "reason" .= show err]
                            Right (DEval.InterpreterOutput (DEval.InterpreterDiff _balsDiff _ops)
                                                        nextPs) -> do
                                nextSs <- return $ Map.insert (ps ^. DEval.persistentNextTxId) s ss
                                MV.putMVar mvar (nextPs, nextSs)
                                (DEval.TransactionId tid) <- return $ (ps ^. DEval.persistentNextTxId)
                                return $ BLC.toStrict $ encode $
                                    object [ "status" .= ("Success" :: T.Text)
                                           , "transId" .= show tid
                                           , "currentState" .= (BLC.unpack $ encodePretty (convertQuery (nextPs ^. DEval.persistentTxes, nextSs)))
                                           ]

            LedgerQueryCmd v -> do
              MV.putMVar mvar (ps, ss)
              res <- return $ runQuery v (ps ^. DEval.persistentTxes, ss)
              return $ BLC.toStrict $ encodePretty res

prettyOpLog :: DEval.Cmd -> String
prettyOpLog (DEval.Cmd from' to' amt' sig') = "Transfer " ++ show (fromRational amt' :: Double) ++ " from " ++ T.unpack from' ++ " to " ++ T.unpack to' ++ " with signature: " ++ T.unpack sig'

prettyLedger :: Map.Map Text Rational -> String
prettyLedger m = unlines $ Map.foldlWithKey (\l k v -> l ++ [T.unpack k ++ ": " ++ show (fromRational v :: Double)]) ["","Account: Amount", "----------------------"] m
