{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable,DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}

module Juno.Hoplite.Heap

    where
--import Language.Hopper.Internal.Core.ANF
import Juno.Hoplite.Types
import qualified Data.Map as Map
import GHC.Generics
import Numeric.Natural
import Data.Typeable
import Control.Monad.Trans.State.Strict as State
import Prelude.Extras
import Control.Monad.Trans.Class as MT
import Control.Monad.Primitive as  Prim
import Control.Monad.IO.Class as MIO
import Juno.Hoplite.STExcept
import Data.Data

--import Bound

--import Data.Text (Text)

-- import  Control.Monad.Free
--import Control.Lens
--import qualified Data.Vector as V

--- This model implementation of the heap is kinda a hack --- Namely that
--- _minMaxFreshRef acts as a kinda heap pointer that is >= RefInMap + 1
data Heap ast  =  Heap { _minMaxFreshRef :: !Ref,  _theHeap :: ! (Map.Map Ref (HeapVal ast  ))   }
   deriving (  Typeable ,Generic    )
deriving instance (Functor ast,Show1 ast, Show (ast Ref)) => Show (Heap ast)
deriving instance (Monad ast,Eq1 ast, Eq (ast Ref)) => Eq (Heap ast)
deriving instance (Monad ast, Ord1 ast, Ord (ast Ref)) => Ord (Heap ast)


data HeapError
  = HeapStepCounterExceeded
  | InvalidHeapLookup
  | BlackholeEncounteredDuringLookup
  | HeapLookupOutOfBounds
  deriving (Eq,Ord,Show,Read,Typeable,Generic,Data)

throwHeapError :: MonadTrans t => HeapError -> t (STE (a1 :+ HeapError) s) a
throwHeapError e = lift $ throwSTE $ InR e

heapRefUpdate :: Ref -> HeapVal ast  -> Heap ast  -> HeapStepCounterM  ast (STE (b :+ HeapError ) s) (Heap ast)
heapRefUpdate ref val (Heap ct mp)
        | ref < ct  && ref `Map.member` mp = return $ Heap ct $ Map.insert ref val mp
        | ref >= ct = throwHeapError HeapLookupOutOfBounds -- error $ "impossible heap ref greater than heap max, deep invariant failure" ++ show ref
        | otherwise {- invalid heap ref -} = throwHeapError InvalidHeapLookup

heapAllocateValue :: Heap ast   -> HeapVal ast   -> (Ref,Heap ast  )
heapAllocateValue hp val = (_minMaxFreshRef hp
                            , Heap (Ref $ (refPointer minmax) + 1) newMap)
  where
      minmax = _minMaxFreshRef hp
      newMap = Map.insert minmax  val (_theHeap hp)

data CounterAndHeap ast  =  CounterAndHeap {
                                        _extractCounterCAH :: !Natural
                                        -- this should be a Natural, represents  number of
                                        -- steps left
                                        ,_extractHeapCAH :: !(Heap ast ) }
                            deriving (

                                      Typeable

                                      ,Generic

                                      --,Foldable
                                      --,Traversable
                                      --,Functor
                                      )
deriving instance (Show (ast Ref),Functor ast , Show1 ast ) => Show (CounterAndHeap ast)
deriving instance (Eq (ast Ref),Monad ast,Eq1 ast ) => Eq (CounterAndHeap ast)
deriving instance (Ord (ast Ref), Monad ast, Ord1 ast) => Ord (CounterAndHeap ast)

extractHeapCAH :: Functor f => ((Heap ast  ) ->  f (Heap ast  ))
                  -> CounterAndHeap ast    -> f (CounterAndHeap ast  )
extractHeapCAH fun cnh = fmap (\mp' -> cnh{_extractHeapCAH=mp'}) $ fun $ _extractHeapCAH cnh

extractCounterCAH :: Functor f => (Natural -> f Natural )-> (CounterAndHeap ast   -> f (CounterAndHeap ast  ))
extractCounterCAH  fun cnh = fmap (\i' -> cnh{_extractCounterCAH=i'}) $ fun $ _extractCounterCAH cnh

newtype HeapStepCounterM ast  m a = HSCM {_xtractHSCM :: State.StateT  (CounterAndHeap ast ) m a}
   deriving (Typeable,Functor,Generic)

instance MonadIO m => MonadIO (HeapStepCounterM ast m) where
  liftIO m = lift $ MIO.liftIO m

instance PrimMonad m => PrimMonad (HeapStepCounterM ast m) where
  type PrimState (HeapStepCounterM ast m) = Prim.PrimState m
  primitive stfun = lift $ Prim.primitive stfun
instance MT.MonadTrans (HeapStepCounterM ast) where
    lift m =  HSCM $ StateT (\ s -> fmap (\i -> (i,s)) m)
instance Monad  n=>Applicative (HeapStepCounterM ast  n) where
    pure  = \v ->  HSCM $ pure v
    (<*>) = \ (HSCM f) (HSCM v) -> HSCM $ f <*> v
instance Monad m => Monad (HeapStepCounterM ast m) where
    return = pure
    (>>=)= \ (HSCM mv) f -> HSCM (mv  >>= (_xtractHSCM. f))

getHSCM ::Monad m => HeapStepCounterM ast  m (CounterAndHeap ast )
getHSCM  = HSCM State.get

setHSCM ::Monad m =>  CounterAndHeap ast   -> HeapStepCounterM  ast  m  ()
setHSCM v = HSCM $ State.put  v



checkedCounterDecrement ::   HeapStepCounterM  ast  (STE (b :+ HeapError ) s) ()
checkedCounterDecrement = do  cah <- getHSCM
                              ct <- return $  _extractCounterCAH cah
                              if ct <= 0
                                then throwHeapError HeapStepCounterExceeded-- error "allowed step count exceeded, aborting"
                                else setHSCM cah{_extractCounterCAH = ct - 1}

unsafeHeapUpdate :: Ref -> HeapVal ast  -> HeapStepCounterM ast (STE (b :+ HeapError ) s) ()
unsafeHeapUpdate rf val = do  cah <- getHSCM
                              x <-  heapRefUpdate rf val (_extractHeapCAH cah)
                              checkedCounterDecrement
                              x `seq` setHSCM $ cah{_extractHeapCAH =x }

--- note, this should also decrement the counter!
heapAllocate :: HeapVal  ast  -> HeapStepCounterM  ast  (STE (b :+ HeapError ) s) Ref
heapAllocate val = do   cah <-  getHSCM
                        (rf,hp) <- pure $ heapAllocateValue (_extractHeapCAH cah) val
                        cah' <- pure $ cah{_extractHeapCAH = hp}
                        checkedCounterDecrement
                        setHSCM cah'
                        return rf

heapLookup :: Ref -> HeapStepCounterM ast (STE (b :+ HeapError) s) (HeapVal ast)
heapLookup ref = do
  checkedCounterDecrement
  heapHandle <- _extractHeapCAH <$> getHSCM
  heapRefLookup ref heapHandle
   where
     heapRefLookup :: Ref -> Heap ast -> HeapStepCounterM ast (STE (b :+ HeapError) s) (HeapVal ast)
     heapRefLookup rf (Heap ct mp)
       | ref < ct && rf `Map.member` mp = return $ mp Map.! rf
       | ref >= ct = throwHeapError HeapLookupOutOfBounds
       | otherwise {- invalid heap ref -} = throwHeapError InvalidHeapLookup


--- this doesn't validate Heap and heap allocator correctness, VERY UNSAFE :)
unsafeRunHSCM :: Monad m =>  Natural -> Heap ast  -> HeapStepCounterM ast m b -> m (b,CounterAndHeap ast  )
unsafeRunHSCM cnt hp (HSCM m)  = State.runStateT m (CounterAndHeap cnt hp)

-- run a program in an empty heap
runEmptyHeap :: Monad m =>  Natural -> HeapStepCounterM ast m  b-> m (b,CounterAndHeap ast )
runEmptyHeap ct (HSCM m) = State.runStateT m (CounterAndHeap ct $ Heap (Ref 1) Map.empty)

heapRefLookupTransitive :: Ref -> HeapStepCounterM ast (STE (b :+ HeapError) s) (HeapVal ast, Ref)
heapRefLookupTransitive ref = do
  next <- heapLookup ref
  case next of
    BlackHoleF -> throwHeapError BlackholeEncounteredDuringLookup
    IndirectionF nextRef -> heapRefLookupTransitive nextRef
    val -> return (val, ref)
