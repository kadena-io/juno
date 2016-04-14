{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE DeriveAnyClass #-}-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric, TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Juno.Hoplite.Eval where


-- import qualified  Language.Hopper.Internal.Core.TermDemoWare as TDW
import Juno.Hoplite.Heap
import  Control.Monad.RWS.Class
import Control.Monad.RWS.Strict

-- import Control.Monad.Trans (lift)
import Data.Typeable
import Data.Data
import GHC.Generics
import qualified Data.Map.Strict as Map
import Numeric.Natural
import Data.Text (Text,pack)
import Bound
import Juno.Hoplite.Types
import Control.Lens (view,_1,makeLenses)
-- import Text.Read (readMaybe)
-- import Control.Monad.Error.Class (throwError)
import Juno.Hoplite.STExcept

{-
reader holds the "snapshot" of the state of all accounts/balances before this program is run and thence committed
state holds BOTH an overlay map on top of the snapshot of any accounts whose balances have been adjusted
  so far
  AND a Natural number counter of # of (implicilty successful) commands written to proposed diff so far
writer holds the "log" of commands written so far, paired with an index that for now will be the #
  of commands previously written to the log in that execution (ie it defines the valid ordering for commiting those commands)

NOTE: does writer class allow disrupting the log??
-}

--  programs will just be [Cmd]
-- and we desugar those to nested lets whose names we ignore, and then we run it
{-
[transfer1 ... ]

let _ = transfer1
  in
    let _ = ...
        ...

-}

data DemoError = DERRRR Text
  deriving (Eq,Ord,Typeable,Show)

data Cmd = Cmd {from :: Text , to :: Text , posAmount :: Rational , fakeCryptoSig:: Text }
  deriving (Eq,Ord,Show,Read,Data,Typeable)

-- readCmdMaybe :: String -> Maybe Cmd
-- readCmdMaybe str =  (\ (fr,t,pos,fk) -> Cmd fr t pos fk)  <$> readMaybe str
--
-- readCmdListMaybe  :: String -> Maybe [Cmd]
-- readCmdListMaybe str = fmap (fmap (\ (fr,t,pos,fk) -> Cmd fr t pos fk) ) $ readMaybe str

data ExpContext  ty a  = SCEmpty
                        | LetContext
                            (Maybe Text) -- we're not currently using / needing the variable name here
                            (Scope (Maybe Text) (Exp ty) a)
                            (ExpContext ty a)
                        -- | ThunkUpdate !a (ExpContext ty a)
                        | FunAppCtxt  [Ref] [Exp ty a] (ExpContext  ty a)
                          -- lets just treat the ref list as having the function ref at the
                          -- "bottom of the stack", when [Exp] is empty, reverse ref list to resolve function and apply args
                        | PrimAppCtxt  PrimOpId [Ref] [Exp ty a] (ExpContext  ty a)

                        -- applications are their one hole contexts!
   deriving (Typeable
          ,Functor
          ,Foldable
          ,Traversable
          ,Generic
          ,Data
          ,Eq
          ,Ord
          ,Show)


data InterpreterError
  = PrimopTypeMismatch
  | NonClosureInApplicationPosition
  | ArityMismatchFailure
  | HeapLookupFailure
  | MalformedClosure
  | MismatchedStackContext
  | PrimFailure String
  | UnsupportedTermConstructFailure String
  deriving (Eq,Ord,Show,Typeable,Data)

newtype TransactionId = TransactionId Natural
  deriving (Eq, Show, Num, Enum, Ord)

newtype OpId = OpId Natural
  deriving (Eq, Show, Num, Enum, Ord)

type OrderedOp = (OpId, Cmd)

data PersistentState
  = PersistentState { _persistentNextTxId :: TransactionId
                    , _persistentBalances :: Map.Map Text Rational
                    , _persistentTxes :: [(TransactionId, [OrderedOp])]
                    }
  deriving (Eq, Show)

data InterpreterOverlay
  = InterpreterOverlay { _overlayNextOpId :: OpId
                       , _overlayBalances :: Map.Map Text Rational
                       }
  deriving (Eq, Show)

newtype BalancesDiff
  = BalancesDiff { _balancesDiff :: Map.Map Text (Rational, Rational) } -- (before, after)
  deriving (Eq, Show)

data InterpreterDiff
  = InterpreterDiff { _interpDiffBalances :: BalancesDiff
                    , _interpDiffOps :: [OrderedOp] }
  deriving (Eq, Show)

data InterpreterOutput
  = InterpreterOutput { _outputDiff :: InterpreterDiff
                      , _outputNextState :: PersistentState
                      }
  deriving (Eq, Show)

$(makeLenses ''PersistentState)
$(makeLenses ''InterpreterOverlay)
$(makeLenses ''BalancesDiff)
$(makeLenses ''InterpreterDiff)
$(makeLenses ''InterpreterOutput)

initialState :: PersistentState
initialState = PersistentState 0 Map.empty []

emptyOverlay :: InterpreterOverlay
emptyOverlay = InterpreterOverlay 0 Map.empty

--
-- TODO: test mkOutput
--
mkOutput :: InterpreterOverlay -> [OrderedOp] -> PersistentState -> InterpreterOutput
mkOutput (InterpreterOverlay _ overlayBals) ops (PersistentState txId bals txes) =
  InterpreterOutput diff nextState
    where
      balChanges = Map.intersectionWith (\ a b -> (a, b)) bals overlayBals
      diff = InterpreterDiff (BalancesDiff balChanges) ops
      nextState = PersistentState (succ txId)
                                  (overlayBals `mappend` bals)
                                  ((txId, ops) : txes)

runExpr :: Natural
        -> PersistentState
        -> (forall v . Exp () v) -- guarantees we have no free variables
        -> Either (() :+ InterpreterError :+ HeapError)
                  InterpreterOutput
runExpr step st expr = fmap projectOutput
                     $ handleSTE id
                     $ runEmptyHeap step
                     $ runRWST (evalExp SCEmpty expr) env emptyOverlay
  where
    env = _persistentBalances st
    projectOutput ((_heapVal, overlay, ops), _heap) = mkOutput overlay ops st

type InterpStack s ty b a
  = RWST (Map.Map Text Rational)
         [OrderedOp]
         InterpreterOverlay
         (HeapStepCounterM (Exp ty)
                           (STE (b :+ InterpreterError :+ HeapError) s))
         a

-- should this be ref or value in the return position? lets revisit later
-- maybe it should be (Ref,HeapVal (Exp ty))  in return position?
evalExp :: (Ord ty,Show ty)=>  ExpContext ty Ref -> Exp ty Ref
  -> InterpStack s  ty b ((HeapVal (Exp ty)), Ref)
evalExp stk (V rf) =  do  rp@(_hpval,_ref) <- lift $ heapRefLookupTransitive rf
                          applyStack stk rp -- does this need both heap location and value in general?
-- should Force reduce both Thunk e AND (Thunk (Thunk^+ e)) to e? for now I claim YES :)
evalExp _stk (Force _e)= throwInterpError $ UnsupportedTermConstructFailure  "Force" {-do  res <- evalExp stk e
                            forcingApply stk res -}-- forcing apply inits ref to thunk when evaluated to blackholeF
evalExp _stk (Delay _e) = throwInterpError $ UnsupportedTermConstructFailure "Delay" --- do ref <- lift  $heapAllocate (ThunkF e) ; applyStack stk (ThunkF e,ref)
evalExp stk (funE :@ args) = evalClosureApp (FunAppCtxt [] (funE:args) stk )
evalExp stk (ELit l) =  do ref <- lift $  heapAllocate (VLitF l) ; applyStack stk (VLitF l, ref)
evalExp stk (PrimApp name args) = evalPrimApp (PrimAppCtxt name [] args stk )
evalExp stk (Lam ts bod) = do val <- return $ (DirectClosureF (MkClosure (map (ArityBoxed . view _1 )ts  ) bod))
                              ref <- lift $  heapAllocate val
                              applyStack stk (val,ref)
evalExp stk (Let mv _mty rhsExp scp) = evalExp (LetContext mv scp stk) rhsExp

noBlackholeArgs :: (Ord ty,Show ty) => [Ref] -> InterpStack s  ty b ()
noBlackholeArgs rls = lift $ void $ traverse heapRefLookupTransitive rls

throwInterpError :: InterpreterError
                 -> InterpStack s  ty b a
throwInterpError e =lift $   lift $ throwSTE $ (InL . InR) e

evalClosureApp :: forall ty  s b .  (Show ty, Ord ty)
               => ExpContext ty Ref
               -> InterpStack s  ty b (HeapVal (Exp ty), Ref)
evalClosureApp (FunAppCtxt ls [] stk) = do
  (funRef:argsRef) <- return $ reverse ls
  (val,_ref) <- lift $  heapRefLookupTransitive funRef
  noBlackholeArgs argsRef
  case val of
    (DirectClosureF (MkClosure wrpNames scp))
      | length argsRef == length wrpNames
      -> let nmMap = Map.fromList $ zip (map _extractArityInfo wrpNames) argsRef
             substApply ::  Var Text (Exp ty2 Ref)
                    -> InterpStack s  ty b (Exp ty2 Ref)
             substApply var = case var of
                     (B nm) -> maybe (throwInterpError MalformedClosure)
                                     (\ rf -> lift $ return $ V rf)
                                     (Map.lookup nm nmMap)
                     (F term) -> return term
         in do
           nextExp <-   traverse substApply $ unscope scp
           evalExp stk $ join nextExp
      | otherwise -> throwInterpError ArityMismatchFailure -- error "closure arity vs argument application mismatch"
    _ ->  throwInterpError NonClosureInApplicationPosition  -- error "cannot invoke non closure heap values in application position"
evalClosureApp (FunAppCtxt ls (h : t) stk) = evalExp (FunAppCtxt ls t stk) h
evalClosureApp (LetContext _ _ _ ) =  throwInterpError MismatchedStackContext --error "letcontext appearing where there should be an closure app context"
-- evalClosureApp (ThunkUpdate _ _ ) =  throwInterpError MismatchedStackContext --  error "thunkupdate context appearing where there should be a closure app context"
evalClosureApp (PrimAppCtxt _ _ _ _) =  throwInterpError MismatchedStackContext -- error "prim app context appearing where there should be a closure app context"
evalClosureApp SCEmpty  = throwInterpError MismatchedStackContext -- error "empty stack where application context is expected"

evalPrimApp ::(Show ty, Ord ty) => ExpContext ty Ref -> InterpStack s  ty b ((HeapVal (Exp ty)), Ref)
evalPrimApp (PrimAppCtxt nm args [] stk) =  do   noBlackholeArgs args ;  applyPrim stk nm $ reverse args
evalPrimApp (PrimAppCtxt nm args (h:t) stk) = evalExp (PrimAppCtxt nm  args t stk) h
evalPrimApp (LetContext _ _ _ ) = throwInterpError MismatchedStackContext -- error "letcontext appearing where there should be an prim app context"
-- evalPrimApp (ThunkUpdate _ _ ) = throwInterpError MismatchedStackContext-- error "thunkupdate context appearing where there should be a prim app context"
evalPrimApp (FunAppCtxt _ _ _) = throwInterpError MismatchedStackContext -- error "fun app context appearing where there should be a prim app context"
evalPrimApp SCEmpty  = throwInterpError MismatchedStackContext --- error "empty stack where prim app context is expected"

forcingApply :: (Ord ty,Show ty)=>
    ExpContext ty Ref -> (HeapVal (Exp ty),Ref) ->InterpStack s  ty b ((HeapVal (Exp ty)), Ref)
forcingApply = undefined

--- question: do we need to guard from blackholes in substitution points??????
applyStack :: (Ord ty,Show ty)=>
    ExpContext ty Ref -> (HeapVal (Exp ty),Ref)
      -> InterpStack s  ty b ((HeapVal (Exp ty)), Ref)
applyStack SCEmpty p= return p
applyStack (LetContext _mv scp stk) (_v,ref)  = evalExp stk (instantiate1 (V ref) scp)
applyStack (FunAppCtxt ls [] stk) (_,ref) = evalClosureApp  (FunAppCtxt (ref : ls) [] stk)
applyStack (FunAppCtxt ls (h:t) stk) (_,ref) = evalExp (FunAppCtxt (ref:ls) t stk) h
applyStack (PrimAppCtxt nm revArgs [] stk) (_,ref) = evalPrimApp (PrimAppCtxt nm (ref:revArgs) [] stk)
applyStack (PrimAppCtxt nm revargs (h:t) stk) (_,ref) = evalExp (PrimAppCtxt nm (ref : revargs) t stk) h
-- applyStack (ThunkUpdate thunkRef stk) pr@(_val,ref) = do
  -- NOTE: under our current (deterministic, sequential) semantics, thunkRef
  --       *should* (transitively) lead to a blackhole before update.
  -- lift $ unsafeHeapUpdate thunkRef (IndirectionF ref)
  -- applyStack stk pr






lookupPrimAccountBalance :: (Ord ty,Show ty)=> Text ->  InterpStack s  ty b (Maybe Rational)
lookupPrimAccountBalance acctNam = do
      InterpreterOverlay _nextOpId localizedMap <- get
      case Map.lookup  acctNam localizedMap of
          Just v |  v >= 0 -> return $ Just  v
                 | otherwise -> throwInterpError $ PrimFailure "critical data invariant failure in underlying snapshot or localized map"
          Nothing -> do
              snapshotMap :: (Map.Map Text Rational ) <- ask
              case Map.lookup  acctNam snapshotMap of
                Just a | a >= 0 -> return $ Just  a
                       | otherwise ->  throwInterpError $ PrimFailure   "critical data invariant in base snapshot of account balance data "
                Nothing ->  return Nothing  -- throwError $ "failure to find account balance for " ++ show acctNam

-- this may abort if target account doesn't exist
updatePrimAccountBalanceByAdding :: (Ord ty,Show ty)=> Text -> Rational ->  InterpStack s  ty b ()
updatePrimAccountBalanceByAdding nm amt = do
      currentBalance <- lookupPrimAccountBalance nm
      case currentBalance of
          Nothing   -> throwInterpError $ PrimFailure "account doesn't exist "
          Just current |  current + amt < 0 -> throwInterpError $ PrimFailure  "cant debit an account more than its current balance"
                       |  otherwise -> do
                            InterpreterOverlay nextOpId localizedMap <- get
                            put $ InterpreterOverlay nextOpId $ (Map.insert nm $! (current + amt)) localizedMap


applyPrim :: (Ord ty,Show ty)=> ExpContext ty Ref  -> PrimOpId -> [Ref]
  ->  InterpStack s  ty b ((HeapVal (Exp ty)), Ref)
applyPrim ctxt opid argsRef = do  resPair <- applyPrimDemo opid argsRef ; applyStack ctxt resPair

applyRationalArithmetic :: (Ord ty, Show ty)
                        => (Rational -> Rational -> Rational)
                        -> [Ref]
                        -> InterpStack s ty b (HeapVal (Exp ty), Ref)
applyRationalArithmetic f [ref1, ref2] = do
  (ratLit1, _posRatRef) <- lift $ heapRefLookupTransitive ref1
  (ratLit2, _posRatRef) <- lift $ heapRefLookupTransitive ref2
  case (ratLit1, ratLit2) of
    (VLitF (LRational rat1), VLitF (LRational rat2)) -> do
      val <- return $ VLitF $ LRational $ rat1 `f` rat2
      ref <- lift $ heapAllocate val
      return (val,ref)

    b -> error $ "deep invariant failure: bad args to arithmetic primop, the arguments were" ++ show b
applyRationalArithmetic _ refs = error $ "deep invariant failure: wrong number of args to arithmetic primop (2 instead of " ++ show (length refs) ++ ")"

applyPrimDemo :: (Ord ty,Show ty)=> PrimOpId -> [Ref] ->  InterpStack s  ty b  ((HeapVal (Exp ty)), Ref)
applyPrimDemo (PrimopId "transfer") [fromRef,toRef,posRatRef,fakeCryptoSigRef] = do
  (fromVal,_reffrom) <-  lift $ heapRefLookupTransitive fromRef
  (toVal,_refto) <- lift $ heapRefLookupTransitive toRef
  (posRatVal,_posRatRef) <- lift  $ heapRefLookupTransitive posRatRef
  (fakeSigVal,_fakeSigRef) <- lift $ heapRefLookupTransitive fakeCryptoSigRef
  case (fromVal,toVal,posRatVal,fakeSigVal) of
    (VLitF (LText fromNm), VLitF (LText toNm), VLitF (LRational amt), VLitF (LText demoSig))
      | amt < 0 -> error "transfer command was invoked with a negative amount"
      | otherwise -> do
          sourceBalanceM <- lookupPrimAccountBalance fromNm
          targetBalanceM <- lookupPrimAccountBalance toNm
          case (sourceBalanceM,targetBalanceM) of
              (Just srcB,Just _targetB) ->
                  if srcB >= amt then
                    do
                      updatePrimAccountBalanceByAdding fromNm (-amt)
                      updatePrimAccountBalanceByAdding toNm amt
                      InterpreterOverlay nextOpId mp <- get
                      tell [(nextOpId, Cmd fromNm toNm amt demoSig)]
                      put $ InterpreterOverlay (succ nextOpId) mp
                      val <- return $ VLitF  $ LText $  pack "success"
                      ref <- lift $ heapAllocate val  -- this shoudld be unit, but whatever
                      return (val,ref)
                    else  throwInterpError $ PrimFailure   "source balance is less than amount to be transfered"
              bad ->  throwInterpError $ PrimFailure   $ "invalid account(s)" ++ show (fromNm, toNm)

    b -> error $ "deep invariant failure : bad args to transfer primop, the arguments were" ++ show b
applyPrimDemo (PrimopId "+") refs = applyRationalArithmetic (+) refs
applyPrimDemo (PrimopId "-") refs = applyRationalArithmetic (-) refs
applyPrimDemo (PrimopId "*") refs = applyRationalArithmetic (*) refs
applyPrimDemo (PrimopId "/") refs = applyRationalArithmetic (/) refs
applyPrimDemo a b = error $ "called command  " ++ show  a ++ " with "  ++ show  (length b)   ++ " arguments"
