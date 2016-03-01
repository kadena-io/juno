{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Juno.Messaging.NanoMsg (
  runMsgServer
  ) where

runMsgServer :: IO ()
runMsgServer = undefined

{- Commented out so that you don't need to install nanomsg to install

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan.Unagi
import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Serialize

import Nanomsg

import Network.Tangaroa.Messaging

receiverProcess :: Serialize a => InChan a -> Addr String -> IO ()
receiverProcess inbox (Addr addr) =
  withSocket Pull $ \sock -> do
    _ <- bind sock addr
    forever $ do
      msg <- recv sock
      case decode msg of
        Left err -> error err
        Right v -> writeChan inbox v


senderProcess :: Serialize a => OutChan (OutBoundMsg String a) -> Rolodex String (Socket Push) -> IO ()
senderProcess outboxRead rol@(Rolodex r) = do
  (OutBoundMsg toAddrs msg) <- readChan outboxRead
  rol' <- case toAddrs of
    RAll -> return rol
    RSome addrs -> if addrs `Set.isSubsetOf` Map.keysSet r
                   then return rol
                   else addNewAddrs rol $ Set.toList addrs
    ROne addr -> if Set.member addr $ Map.keysSet r
                   then return rol
                   else addNewAddrs rol [addr]
  handleSend msg $ recipList rol' toAddrs
  senderProcess outboxRead rol'

handleSend :: Serialize a => a -> [Socket Push] -> IO () -- Rolodex -> IO ()
handleSend msg socks = do
  mapM_ (forkIO . sendMsg (encode msg)) socks
  return ()

recipList :: Rolodex String (Socket Push) -> Recipients String -> [Socket Push]
recipList (Rolodex r) RAll = _unListenOn <$> Map.elems r
recipList (Rolodex r) (RSome addrs) = _unListenOn <$> (r Map.!) <$> Set.toList addrs
recipList (Rolodex r) (ROne addr) = _unListenOn <$> [r Map.! addr]

sendMsg :: ByteString -> Socket Push -> IO ()
sendMsg msg sock = send sock msg

addNewAddrs :: Rolodex String (Socket Push) -> [Addr String] -> IO (Rolodex String (Socket Push))
addNewAddrs r [] = return r
addNewAddrs (Rolodex r) (x:xs) = do
  r' <- if Map.member x r
        then return $ Rolodex r
        else do
          s <- socket Push
          _ <- connect s $ _unAddr x
          return $ Rolodex $ Map.insert x (ListenOn s) r
  addNewAddrs r' xs

runMsgServer :: Serialize m => Spec String m (Socket Push) -> IO ()
runMsgServer (Spec inbox outbox me rol) = do
  _ <- forkIO $ receiverProcess inbox me
  _ <- forkIO $ senderProcess outbox rol
  return ()

-}
