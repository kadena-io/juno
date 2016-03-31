module Main (main) where

import Crypto.Random
import Crypto.Ed25519.Pure
import Text.Read
import Juno.Runtime.Types

import System.IO
import System.FilePath
import qualified Data.Yaml as Y

import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

nodes :: [NodeID]
nodes = iterate (\n@(NodeID _ p) -> n {_port = p + 1}) (NodeID "127.0.0.1" 10000)

makeKeys :: CryptoRandomGen g => Int -> g -> [(PrivateKey,PublicKey)]
makeKeys 0 _ = []
makeKeys n g = case generateKeyPair g of
  Left err -> error $ show err
  Right (p,priv,g') -> (p,priv) : makeKeys (n-1) g'

keyMaps :: [(PrivateKey,PublicKey)] -> (Map NodeID PrivateKey, Map NodeID PublicKey)
keyMaps ls = (Map.fromList $ zip nodes (fst <$> ls), Map.fromList $ zip nodes (snd <$> ls))

main :: IO ()
main = do
  putStrLn "Number for config files to generate?"
  hFlush stdout
  mn <- fmap readMaybe getLine
  case mn of
    Just n  -> do
      g <- newGenIO :: IO SystemRandom
      keyMaps' <- return $! keyMaps $ makeKeys n g
      confs <- return ((`createConfig` keyMaps') <$> take n nodes)
      mapM_ (\c -> Y.encodeFile ("conf" </> show (_port $ _nodeId c) ++ ".yaml") c) confs
    Nothing -> putStrLn "Please specify a number of keys to generate."

createConfig :: NodeID -> (Map NodeID PrivateKey, Map NodeID PublicKey) -> Config
createConfig nid (privMap, pubMap) = Config
  { _otherNodes           = Set.delete nid $ Map.keysSet pubMap
  , _nodeId               = nid
  , _publicKeys           = pubMap
  , _clientPublicKeys     = pubMap
  , _myPrivateKey         = privMap Map.! nid
  , _myPublicKey          = pubMap Map.! nid
  , _electionTimeoutRange = (3000000,6000000)
  , _heartbeatTimeout     = 1500000
  , _enableDebug          = True
  , _clientTimeoutLimit   = 50000
  }
