module Main (main) where

import Codec.Crypto.RSA
import Crypto.Random
import Text.Read
import Network.Socket
import Data.Word
import System.Directory
import System.IO
import System.FilePath

import qualified Data.Map as Map

portnums :: [Word16]
portnums = iterate (+ 1) 10000

main :: IO ()
main = do
  g <- newGenIO :: IO SystemRandom
  putStrLn "Number of keys to generate? "
  hFlush stdout
  mn <- fmap readMaybe getLine
  case mn of
    Just n  -> do
      let keys = generateKeys g 1024 n
      writePublicKeys $ map fst keys
      writePrivateKeys $ zip portnums $ map snd keys
    Nothing -> putStrLn "Please specify a number of keys to generate."

generateKeys :: CryptoRandomGen g => g -> Int -> Int -> [(PublicKey, PrivateKey)]
generateKeys g nbits nkeys = case nkeys of
  0 -> []
  n -> (pubkey, privkey) : generateKeys ng nbits (n - 1) where
    (pubkey, privkey, ng) = generateKeyPair g nbits

localhost :: HostAddress
localhost = 0x0100007f

writePublicKeys :: Show a => [a] -> IO ()
writePublicKeys xs = do
  putStrLn "Filename for public keys? "
  hFlush stdout
  filename <- getLine
  writeFile filename $
    show $ Map.fromList $
      zip
        (zip
          (repeat localhost)
          portnums)
        xs

writePrivateKeys :: (Show name, Show a) => [(name,a)] -> IO ()
writePrivateKeys xs = do
  putStrLn "Folder for private keys? "
  hFlush stdout
  dirname <- getLine
  createDirectory dirname
  mapM_ (\(fn, x) -> writeFile (dirname </> show fn ++ ".txt") (show x)) xs
