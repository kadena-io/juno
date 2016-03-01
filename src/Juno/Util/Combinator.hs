{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module Juno.Util.Combinator
  ( (^$)
  , (^=<<.)
  ) where

import Control.Lens
import Control.Monad.RWS

-- like $, but the function is a lens from the reader environment with a
-- pure function as its target
infixr 0 ^$
(^$) :: forall (m :: * -> *) b r a. (MonadReader r m, Functor m) =>
  Getting (a -> b) r (a -> b) -> a -> m b
lf ^$ a = fmap ($ a) (view lf)

infixr 0 ^=<<.
(^=<<.) :: forall a (m :: * -> *) b r s.
  (MonadReader r m, MonadState s m) =>
  Getting (a -> m b) r (a -> m b) -> Getting a s a -> m b
lf ^=<<. la = view lf >>= (use la >>=)
