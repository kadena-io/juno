{-# LANGUAGE MagicHash, UnboxedTuples, RankNTypes, TypeFamilies, DeriveDataTypeable, GADTs,FlexibleContexts #-}
module Juno.Hoplite.STExcept(
  STE
  ,runSTE
  ,throwSTE
  ,handleSTE
  )

  where

-- import qualified  GHC.ST as GST
import GHC.Prim (State#,realWorld#,Any)
import Control.Exception as Except
import Control.Monad (ap)
import Control.Monad.Primitive
import Data.Typeable
import Unsafe.Coerce (unsafeCoerce)

-- maybe this constructor shouldn't be private?
newtype STE e s a = STE  (STRep s a)
type STRep s a = State# s -> (# State# s, a #)

instance Functor (STE e s) where
    fmap f (STE m) = STE $ \ s ->
      case (m s) of { (# new_s, r #) ->
      (# new_s, f r #) }

instance Applicative (STE e s) where
    pure = return
    (<*>) = ap

instance Monad (STE e s) where
    {-# INLINE return #-}
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    return x = STE (\ s -> (# s, x #))
    m >> k   = m >>= \ _ -> k

    (STE m) >>= k
      = STE (\ s ->
        case (m s) of { (# new_s, r #) ->
        case (k r) of { STE k2 ->
        (k2 new_s) }})

instance PrimMonad (STE e s) where
  type PrimState (STE e s) = s
  primitive = STE
  {-# INLINE primitive #-}
instance PrimBase (STE e s) where
  internal (STE p) = p
  {-# INLINE internal #-}

unsafePrimToSTE :: PrimBase m => m a -> STE e s a
{-# INLINE unsafePrimToSTE #-}
unsafePrimToSTE = unsafePrimToPrim

{-# NOINLINE runSTE #-} -- this may not be needed and may make code closer when its a small STE computation (though we're using it for small stuff )
runSTE :: (forall s. STE e s a) -> (Either e a  -> b) -> b
runSTE st  f = runSTRep (case  do  res <-  unsafePrimToSTE $ catch   (unsafePrimToPrim $ fmap Right  st)  (\(STException err) -> return (Left  $ unsafeCoerce err)) ; return (f res) of { STE st_rep -> st_rep })

handleSTE :: (Either e a -> b) -> (forall s. STE e s a)  -> b
handleSTE f st = runSTE st f

{-#  NOINLINE throwSTE #-} -- again audit
throwSTE ::  e -> STE e s a
throwSTE err = unsafePrimToSTE (throwIO (STException  $ unsafeCoerce err))

-- I'm only letting runSTRep be inlined right at the end, in particular *after* full laziness
-- That's what the "INLINE [0]" says.
--              SLPJ Apr 99
-- {-# INLINE [0] runSTRep #-}

-- SDM: further to the above, inline phase 0 is run *before*
-- full-laziness at the moment, which means that the above comment is
-- invalid.  Inlining runSTRep doesn't make a huge amount of
-- difference, anyway.  Hence:

{-# NOINLINE runSTRep #-}
runSTRep :: (forall s. STRep s a) -> a
runSTRep st_rep = case st_rep realWorld# of
                        (# _, r #) -> r


data STException   where
   STException :: Any -> STException
  deriving(Typeable)
instance Show (STException) where
  show (STException _) = "(STException  <OPAQUE HEAP REFERENCE HERE>)"
instance  Exception STException

-- runSTFree :: Typeable e => (forall . STE (W e) s a) -> (Either e a -> b) -> b
