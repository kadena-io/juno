module Main (main) where

import Crypto.Sign.Ed25519
import Control.Monad
import Text.Read
import Juno.Runtime.Types (NodeID(..))
import System.Directory
import System.IO
import System.FilePath

import qualified Data.Map as Map

nodes :: [NodeID]
nodes = iterate (\n@(NodeID _ p) -> n {_port = p + 1}) (NodeID "127.0.0.1" 10000)

main :: IO ()
main = do
  putStrLn "Number of keys to generate? "
  hFlush stdout
  mn <- fmap readMaybe getLine
  case mn of
    Just n  -> do
      keys <- replicateM n createKeypair
      writePublicKeys $ map fst keys
      writePrivateKeys $ zip nodes $ map snd keys
    Nothing -> putStrLn "Please specify a number of keys to generate."

writePublicKeys :: Show a => [a] -> IO ()
writePublicKeys xs = do
  putStrLn "Filename for public keys? "
  hFlush stdout
  filename <- getLine
  writeFile filename $
    show $ Map.fromList $
      zip nodes xs

writePrivateKeys :: (Show a) => [(NodeID,a)] -> IO ()
writePrivateKeys xs = do
  putStrLn "Folder for private keys? "
  hFlush stdout
  dirname <- getLine
  createDirectory dirname
  mapM_ (\(NodeID _ fn, x) -> writeFile (dirname </> show fn ++ ".txt") (show x)) xs
