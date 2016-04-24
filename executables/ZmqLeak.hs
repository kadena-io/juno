{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

import Control.Concurrent
import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi
import Control.Monad (forever, void, when)
import System.ZMQ4.Monadic
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import System.Random

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)

newtype Addr a = Addr { _unAddr :: a } deriving (Read,Show,Eq,Ord)
newtype Rolodex a s = Rolodex {_unRolodex :: Map.Map (Addr a) (ListenOn s)}
newtype ListenOn a = ListenOn {_unListenOn :: a}

-- | Specifiy who the message should go to
data Recipients a = RAll
                  | RSome (Set.Set (Addr a))
                  | ROne (Addr a)
                  deriving (Show,Eq,Generic)

data OutBoundMsg addr msg = OutBoundMsg {
  _obmTo     :: Recipients addr
  , _obmBody :: msg
  } deriving (Show, Eq)

randomStr :: IO ByteString
randomStr = do
  g <- newStdGen
  return $ BS.pack $ take 10 $ randomRs ('a','z') g

randomByteString:: IO ByteString
randomByteString = do
  rs <- randomStr
  return $ BS.concat $ replicate 100 rs

addrList :: [Addr String]
addrList = Addr <$> ["tcp://127.0.0.1:1000","tcp://127.0.0.1:1001","tcp://127.0.0.1:1002"]

mkMsgs :: Unagi.InChan (OutBoundMsg String ByteString) -> Int -> IO ()
mkMsgs _ 0 = return ()
mkMsgs m c = do
  b <- randomByteString
  Unagi.writeChan m $ OutBoundMsg (ROne $ Addr "tcp://127.0.0.1:1000") b
  Unagi.writeChan m $ OutBoundMsg (ROne $ Addr "tcp://127.0.0.1:1001") b
  Unagi.writeChan m $ OutBoundMsg (ROne $ Addr "tcp://127.0.0.1:1002") b
  when (c `mod` 100 == 0) $ putStrLn $ (show c) ++ ": " ++ (show $ BS.take 10 b)
  mkMsgs m (c-1)

main :: IO ()
main = do
  (i,o) <- Unagi.newChan 3
  void $ forkIO $ runZMQ $ do
    sock <- socket Pull
    _ <- bind sock "tcp://127.0.0.1:1003"
    forever $ do
      newMsg <- receive sock
      liftIO $ print newMsg
  threadDelay 10000
  void $ forkIO $ runZMQ $ do
    rolodex <- addNewAddrs (Rolodex Map.empty) addrList
    void $ sendProcess o rolodex
  putStrLn "Start"
  threadDelay 10000
  mkMsgs i 100000
  threadDelay 10000

sendProcess :: Unagi.OutChan (OutBoundMsg String ByteString)
            -> Rolodex String (Socket z Push)
            -> ZMQ z ()
sendProcess outboxRead !r = do
  rMvar <- liftIO $ newMVar r
  forever $ do
    (OutBoundMsg !addrs !msg) <- liftIO $! Unagi.readChan outboxRead
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
          _ <- setConflate True s
          _ <- connect s $ _unAddr x
          return $! Rolodex $! Map.insert x (ListenOn s) r
  r' `seq` addNewAddrs r' xs

recipList :: Rolodex String (Socket z Push) -> Recipients String -> ZMQ z [Socket z Push]
recipList (Rolodex r) RAll = return $! _unListenOn <$> Map.elems r
recipList (Rolodex r) (RSome addrs) = return $! _unListenOn . (r Map.!) <$> Set.toList addrs
recipList (Rolodex r) (ROne addr) = return $! _unListenOn <$> [r Map.! addr]
