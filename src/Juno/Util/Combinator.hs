{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module Juno.Util.Combinator
  ( (^$)
  , (^=<<.)
  , foreverRetry
  ) where

import System.IO (hFlush, stderr, stdout)
import Control.Concurrent (threadDelay, forkFinally, MVar(..), putMVar, takeMVar, newEmptyMVar, forkIO)
import Control.Concurrent.Async
import Control.Lens
import Control.Monad.RWS.Strict
import Data.Thyme.Calendar (showGregorian)
import Data.Thyme.LocalTime

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

prettyThreadDetails :: String -> IO ()
prettyThreadDetails msg = do
  (ZonedTime (LocalTime d' t') _) <- getZonedTime
  putStrLn $ (showGregorian d') ++ "T" ++ (take 15 $ show t') ++ " [THREAD]: " ++ msg
  hFlush stdout >> hFlush stderr

foreverRetry :: String -> IO () -> IO ()
foreverRetry threadName action = void $ forkIO $ forever $ do
  threadDied <- newEmptyMVar
  void $ forkFinally (prettyThreadDetails (threadName ++ " launching") >> action >> putMVar threadDied ())
    $ \res -> do
      case res of
        Right () -> prettyThreadDetails $ threadName ++ " died returning () with no details"
        Left err -> prettyThreadDetails $ threadName ++ " exception " ++ show err
      putMVar threadDied ()
  takeMVar threadDied
  prettyThreadDetails $ threadName ++ "got MVar... restarting"
