{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE BangPatterns #-}

module Juno.Messaging.ZMQ (
  runMsgServer
  ) where

import Control.Concurrent (forkIO, threadDelay, yield, newMVar, takeMVar, putMVar, yield)
import Control.Concurrent.Chan.Unagi
import qualified Control.Concurrent.Chan.Unagi.NoBlocking as NoBlock
import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.ZMQ4.Monadic
import Data.Thyme.Clock
import Data.Serialize

import Juno.Messaging.Types
import Juno.Types (ReceivedAt(..),Digest(..),MsgType(..),SignedRPC(..))

sendProcess :: OutChan (OutBoundMsg String ByteString)
            -> Rolodex String (Socket z Push)
            -> ZMQ z ()
sendProcess outboxRead !r = do
  rMvar <- liftIO $ newMVar r
  forever $ do
    (OutBoundMsg !addrs !msg) <- liftIO $! readChan outboxRead
    r' <- liftIO $ takeMVar rMvar
    !newRol <- updateRolodex r' addrs
    !toPoll <- recipList newRol addrs
    mapM_ (\s -> send s [] msg) toPoll
    liftIO $ putMVar rMvar newRol >> yield

updateRolodex :: Rolodex String (Socket z Push) -> Recipients String -> ZMQ z (Rolodex String (Socket z Push))
updateRolodex r@(Rolodex !_rol) RAll = return $! r
updateRolodex r@(Rolodex !rol) (RSome !addrs) =
  if Set.isSubsetOf addrs $! Map.keysSet rol
  then return $! r
  else do
    !a <- addNewAddrs r $! Set.toList addrs
    return $! a
updateRolodex r@(Rolodex !rol) (ROne !addr) =
  if Set.member addr $! Map.keysSet rol
  then return $! r
  else do
    !a <- addNewAddrs r [addr]
    return $! a

addNewAddrs :: Rolodex String (Socket z Push) -> [Addr String] -> ZMQ z (Rolodex String (Socket z Push))
addNewAddrs !r [] = return r
addNewAddrs (Rolodex !r) (x:xs) = do
  !r' <- if Map.member x r
        then return $! Rolodex r
        else do
          s <- socket Push
--          _ <- setConflate True s -- the client will drop outbound msgs if this is set,
                                    -- once Client no longer uses ZMQ this can be uncommented
          _ <- connect s $ _unAddr x
          return $! Rolodex $! Map.insert x (ListenOn s) r
  r' `seq` addNewAddrs r' xs

recipList :: Rolodex String (Socket z Push) -> Recipients String -> ZMQ z [Socket z Push]
recipList (Rolodex r) RAll = return $! _unListenOn <$> Map.elems r
recipList (Rolodex r) (RSome addrs) = return $! _unListenOn . (r Map.!) <$> Set.toList addrs
recipList (Rolodex r) (ROne addr) = return $! _unListenOn <$> [r Map.! addr]

runMsgServer :: NoBlock.InChan (ReceivedAt, SignedRPC)
             -> NoBlock.InChan (ReceivedAt, SignedRPC)
             -> NoBlock.InChan (ReceivedAt, SignedRPC)
             -> InChan (ReceivedAt, SignedRPC)
             -> OutChan (OutBoundMsg String ByteString)
             -> Addr String
             -> [Addr String]
             -> IO ()
runMsgServer inboxWrite cmdInboxWrite aerInboxWrite rvAndRvrWrite outboxRead me addrList = void $ do
    void $ forkIO $ runZMQ $ do
      sock <- socket Pull
      _ <- bind sock $ _unAddr me
      forever $ do
        newMsg <- receive sock
        ts <- liftIO getCurrentTime
        case decode newMsg of
          Left err -> do
            liftIO $ putStrLn $ "Failed to deserialize to SignedRPC [Msg]: " ++ show newMsg
            liftIO $ putStrLn $ "Failed to deserialize to SignedRPC [Error]: " ++ err
            liftIO yield
          Right s@(SignedRPC dig _)
            | _digType dig == RV || _digType dig == RVR ->
              liftIO $ writeChan rvAndRvrWrite (ReceivedAt ts, s) >> yield
            | _digType dig == CMD || _digType dig == CMDB ->
              liftIO $ NoBlock.writeChan cmdInboxWrite (ReceivedAt ts, s) >> yield
            | _digType dig == AER ->
              liftIO $ NoBlock.writeChan aerInboxWrite (ReceivedAt ts, s) >> yield
            | otherwise           ->
              liftIO $ NoBlock.writeChan inboxWrite (ReceivedAt ts, s) >> yield
    threadDelay 100000 -- to be sure that the recieve side is up first
    forkIO $ runZMQ $ do
      rolodex <- addNewAddrs (Rolodex Map.empty) addrList
      void $ sendProcess outboxRead rolodex
