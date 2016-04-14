{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable,DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators#-}

module Juno.Hoplite.Types where

import Numeric.Natural
import Data.Typeable
import Data.Data
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Foldable
import Prelude.Extras
import GHC.Generics (Generic)
import Data.Word
import qualified Data.Vector as V
import Bound
import Control.Monad
import Data.Traversable

data Literal = LInteger !Integer
             | LRational !Rational
             | LNatural !Natural
             | LText !Text
  deriving(Eq,Ord,Show,Read,Data,Typeable)

newtype ConstrId  = ConstrId { unConstrId :: Text }
    deriving (Eq, Show,Data,Typeable,Ord,Read)

newtype PrimOpId = PrimopId {unPrimopId :: Text}
    deriving (Eq,Show,Data,Typeable,Ord,Read)

data RigModel = Zero | One | Omega
 deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

data Kind = Star | KArr Kind Kind | LiftedPubKey
  deriving (Eq,Ord,Read,Show,Data,Typeable,Generic)

data TCon {-a -}=  TInteger | TNatural | TRational  | TUnit | TArrow RigModel
                | EncryptedFor |  SignedBy
                | PubKey String {- this is not how it'll work :) -}
                -- | Linear
    deriving (Eq,Ord,Read,Show ,Data,Typeable,Generic)
data Type ty  {-a -}=  Tapp (Type ty) (Type ty) | TLit (TCon) | TVar ty
   deriving (Eq1,Ord1,Show1,Read1,Eq,Ord,Read,Show,Data,Typeable,Functor,Foldable,Traversable,Generic)


-- | current theres no pointer tagging in 'Ref' but eventually that will
-- probably change
newtype Ref = Ref {refPointer :: Word64} deriving  (Eq,Show,Ord,Data,Typeable,Generic,Bounded)
refRepLens :: Functor f =>(Word64 -> f a) -> Ref -> f a
refRepLens = \ f (Ref r) -> f r

-- | interface for doing bitwise transformations that yield a new ref
refTransform :: Functor f => (Word64 -> f Word64) -> Ref -> f Ref
refTransform = \ f (Ref r) -> Ref <$> f r

absoluteDistance  :: Ref -> Ref -> Word64
absoluteDistance = \(Ref a) (Ref b) -> if a > b then a - b else b - a

instance Enum Ref where
  succ rf@(Ref w) | rf < maxBound = Ref (1+ w)
                  | otherwise = error $ "succ: Ref overflow"
  pred rf@(Ref w) | rf > minBound = Ref (w - 1)
                  | otherwise = error $ "pred: Ref underflow"
  fromEnum (Ref w)
                | w < fromIntegral (maxBound :: Int) = fromIntegral w
                | otherwise =
                          error "fromEnum: any Ref that is larger than 2^63 -1  is unrepresentable as Int64"
  toEnum n | n >= 0 = Ref $ fromIntegral n
           | otherwise = error "toEnum: Cant represent negative locations in a Ref"


-- | 'Tag' is a constructor tag sum
newtype Tag = Tag { unTag :: Text {-Word64-} } deriving (Eq,Read, Show,Ord,Data,Typeable,Generic)

-- type ValRec  ty   = Free (ValueF  ty Ref) Ref


type HeapVal ast     =  ValueF ast  Ref --  ValueF ty Ref (ValRec ty)

data ValueF ast   v =    VLitF !Literal
              | ConstructorF  !Tag  (WrappedVector v)
              | ThunkF (ast   v )
              --  should this be a heap ref to
              -- closure to have the right sharing ?
              | DirectClosureF (Closure ast  v) -- heap ref?
              | BlackHoleF
              | IndirectionF v
              --- in the types are calling conventions paper,
              --- indirections can point at pointer sized literals, or heap references (or tuples thereof???)

              -- | VRefF !Ref --- refs are so we can have  exlpicit  sharing
                        --- in a manner thats parametric in the choice
                        -- of execution  semantics
   deriving
     (Typeable
      ,Functor
      ,Foldable
      ,Traversable
      ,Generic
      --,Data
      ,Eq
      ,Ord
      ,Show
      ,Read
      --,Eq1
      --,Ord1
      --,Show1
      --,Read1
      )
deriving instance (Data a,Data (ast a),Monad ast,Data (ast (Var Text (ast a))),  Typeable ast,Typeable a)
  =>  Data (ValueF ast a )

instance (Eq1 ast,Monad ast) => Eq1 (ValueF ast) where
    (==#)  (VLitF a) (VLitF b) =  a == b
    (==#)  (VLitF _) _ =  False
    (==#)  (ConstructorF tga va) (ConstructorF tgb vb) =   tga == tgb && va == vb
    (==#)  (ConstructorF _ _) _ =  False
    (==#)  (ThunkF a) (ThunkF b) =  a ==# b
    (==#)  (ThunkF _) _ =  False
    (==#)  (DirectClosureF c1) (DirectClosureF c2) = c1 ==# c2
    (==#)  (DirectClosureF _ ) _ = False
    (==#)  BlackHoleF BlackHoleF = True
    (==#)  BlackHoleF _ =  False
    (==#) (IndirectionF v) (IndirectionF v2) = v == v2
    (==#) (IndirectionF _) _ = False


-- this is a trick to make defining the ord1 instance sane but total
codeValueF :: ValueF a b -> Int
codeValueF (VLitF _) = 1
codeValueF (ConstructorF _ _ ) = 2
codeValueF (ThunkF _) = 3
codeValueF (DirectClosureF _) = 4
codeValueF (BlackHoleF) = 5
codeValueF (IndirectionF _) = 6

instance (Ord1 ast,Monad ast) => Ord1 (ValueF ast) where
  compare1 (VLitF a) (VLitF b) = compare a b
  compare1 a@(VLitF _) b = compare  (codeValueF a) (codeValueF b)
  compare1 (ConstructorF ta va) (ConstructorF tb vb) =
        let tcomp = compare ta tb in  if tcomp == EQ then compare va vb else tcomp
  compare1 a@(ConstructorF _ _) b = compare (codeValueF a) (codeValueF b)
  compare1 (ThunkF e1) (ThunkF e2) = compare1 e1 e2
  compare1 a@(ThunkF _) b = compare (codeValueF a) (codeValueF b)
  compare1 (DirectClosureF a) (DirectClosureF b) = compare1 a b
  compare1 a@(DirectClosureF _) b = compare (codeValueF a) (codeValueF b)
  compare1 BlackHoleF BlackHoleF = EQ
  compare1 a@(BlackHoleF) b = compare (codeValueF a) (codeValueF b)
  compare1 (IndirectionF a) (IndirectionF b) = compare a b  --- this is spiritually evil :))))
  compare1 a@(IndirectionF _ ) b = compare (codeValueF a) (codeValueF b)




newtype WrappedVector a = WrappedVector { unWrappedVector :: V.Vector a }
  deriving (Eq, Show,Ord,Read,Data,Typeable,Functor,Foldable,Traversable)
instance Show1 WrappedVector
instance Eq1 WrappedVector
instance Ord1 WrappedVector
instance Read1 WrappedVector

data Arity = ArityBoxed {_extractArityInfo :: !Text} --- for now our model of arity is boring and simple
                              -- for now lets keep the variable names?
                              -- it'll keep the debugging simpler (maybe?)
 deriving (Eq,Ord,Show,Read,Typeable,Data,Generic)

--- | 'Closure' may need some rethinking ... later
--- they're kinda the erasure of a lambda ... for now
data Closure ast   a = MkClosure ![Arity] !(Scope Text ast a)
  deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable,Generic,Typeable)
deriving instance (Typeable ast,Typeable a,Data (Scope Text ast  a))=> Data (Closure ast   a)
instance (Monad ast, Eq1 ast) => Eq1 (Closure ast)
instance (Monad ast, Ord1 ast) => Ord1 (Closure ast)
instance (Monad ast, Read1 ast) => Read1 (Closure ast)
instance (Monad ast, Show1 ast) => Show1 (Closure ast)



deduceLitKind :: TCon ->  Kind
deduceLitKind tc = case tc of
          TUnit -> Star
          TInteger -> Star
          TNatural -> Star
          TRational -> Star
          -- Linear -> KArr Star Star
          TArrow _ -> KArr Star (KArr Star Star)
          PubKey _s -> LiftedPubKey
          EncryptedFor -> KArr LiftedPubKey (KArr Star Star)
          SignedBy -> KArr LiftedPubKey (KArr Star Star)



wellKindedType ::(Show  ty, Ord ty ) => Map.Map ty Kind -> Type ty -> Either String Kind
wellKindedType kenv tau = case tau of
  TLit tc -> Right $ deduceLitKind tc
  TVar tv -> maybe  (Left $ "free type variable " ++ show tv) Right $ Map.lookup  tv kenv
  Tapp tarr tinput ->
      do  (KArr a b) <- wellKindedType kenv tarr ; c <- wellKindedType kenv tinput ;
          if a == c  {- this part will get tricky later :) -}
              then Right b
              else Left $   "Woops, kind mismatch " ++ show (a,c)

collectFreeVars :: (Ord a, Traversable f) => f a -> Set.Set a
collectFreeVars =   Set.fromList . foldl' (flip (:)) []


data Exp ty a
  = V  a
  | ELit Literal
  -- | PrimApp Text [a]
  | Force (Exp ty a)  --- Force is a Noop on evaluate values,
                      --- otherwise reduces expression to applicable normal form
                      -- should force be more like seq a b, cause pure

  | Delay (Exp ty a)  --- Delay is a Noop on Thunked values, otherwise creates a thunk
                      --- note: may need to change their semantics later?!
  | Exp ty a :@ [Exp ty a]
  | PrimApp  PrimOpId [Exp ty a] -- not sure if this is needed, but lets go with it for now

  | Lam [(Text,Type ty,RigModel)]
        (Scope Text (Exp ty) a)
  | Let (Maybe Text) (Maybe (Type ty,RigModel))  (Exp ty a)  (Scope (Maybe Text) (Exp ty) a) --  [Scope Int Exp a] (Scope Int Exp a)
  deriving (Typeable,Data,Generic)
deriving instance (Read a, Read ty) => Read (Exp ty a)
deriving instance (Read ty) => Read1 (Exp ty)
deriving instance (Show a, Show ty) => Show (Exp ty a)
deriving instance (Show ty) => Show1 (Exp ty)
deriving instance (Ord ty) => Ord1 (Exp ty)
deriving instance (Ord ty,Ord a) => Ord (Exp ty a)
deriving instance (Eq ty) => Eq1 (Exp ty)
deriving instance (Eq a,Eq ty) => Eq (Exp ty a)

instance Functor (Exp ty)  where fmap       = fmapDefault

instance Foldable (Exp ty) where foldMap    = foldMapDefault

instance Applicative (Exp ty) where
  pure  = V
  (<*>) = ap

instance Traversable (Exp ty) where
  traverse f (V a)      = V <$> f a
  traverse _f (ELit e) = pure $ ELit e
  -- traverse f (PrimApp nm ls) = PrimApp nm <$> traverse f ls
  traverse f (Force e) = Force <$> traverse f e
  traverse f (Delay e) = Delay <$> traverse f e
  traverse f (PrimApp nm args) = PrimApp nm <$> traverse (traverse f) args
  traverse f (x :@ ys)   = (:@) <$> traverse f x <*> traverse (traverse f) ys
  traverse f (Lam t e)    = Lam  t <$> traverse f e
  traverse f (Let mname mtype  bs b) = Let  mname mtype <$>  (traverse f) bs <*> traverse f b


instance Monad (Exp ty) where
  -- return = V
  V a         >>= f = f a
  -- PrimApp nm ls >>= f = PrimApp nm (map f ls)
  Delay e     >>= f = Delay $ e >>= f
  Force e     >>= f = Force $ e >>= f
  ELit e      >>= _f = ELit e -- this could also safely be a coerce?
  (x :@ y)    >>= f = (x >>= f) :@ (map (>>= f)  y )
  (PrimApp name args) >>= f = PrimApp name (map (>>= f) args )
  Lam t  e    >>= f = Lam t (e >>>= f)
  Let mname mtype bs  b >>= f = Let mname mtype (  bs >>= f)  (b >>>= f)

-- Smart constructors

type DummyType = Int

abstract' :: (Eq b, Monad f) => [b] -> f b -> Scope b f b
abstract' vs b = abstract (\v' -> if v' `elem` vs
                                    then Just v'
                                    else Nothing)
                          b

lam :: [Text] -> Exp DummyType Text -> Exp DummyType Text
lam vs b = Lam (zipWith (\v n -> (v, TVar n, Omega)) vs [0..])
               (abstract' vs b)

-- | A smart constructor for Lam with one variable
--
-- >>> lam1 "y" (lam1 "x" (V "x" :@ [V "y"]))
-- Lam [("y",TVar 0,Omega)]
--     (Scope (Lam [("x",TVar 0,Omega)]
--            (Scope (V (B "x") :@ [V (F (V (B "y")))]))))
lam1 :: Text -> Exp DummyType Text -> Exp DummyType Text
lam1 v b = lam [v] b

let_ :: Text -> Exp DummyType Text -> Exp DummyType Text -> Exp DummyType Text
let_ v rhs bod = Let (Just v)
                     (Just (TVar 0, Omega))
                     rhs
                     (abstract (\var -> if var == v
                                        then Just (Just v)
                                        else Nothing)
                               bod)

callPrim :: Text -> [Exp ty a] -> Exp ty a
callPrim name = PrimApp (PrimopId name)

infixr 0 !
(!) :: Text -> Exp DummyType Text -> Exp DummyType Text
(!) = lam1

-- let_ "False" ("f" ! "t" ! V"f") (V "False")
-- let_ "True" ("f" ! "t" ! V"t") (V "True")
-- let_ "if" ("b" ! "t" ! "f" ! V"b" :@ [V"f"] :@ [V"t"]) (V "if")
-- let_ "Zero" ("z" ! "s" ! V"z") (V "Zero")
-- let_ "Succ" ("n" ! "z" ! "s" ! V"s" :@ [V"n"]) (V "Succ")
-- let_ "Zero" ("z" ! "s" ! V"z") $
--             let_ "Succ" ("n" ! "z" ! "s" ! V"s" :@ [V"n"]) $
--                         let_ "one" (V"Succ" :@ [V"Zero"]) $
--                                    V "one"


data a :+ b = InL a | InR b
  deriving (Eq,Ord,Show,Typeable,Data,Generic)

infixl 7 :+
