{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
--import Crypto.Sign.Ed25519
import qualified Crypto.Ed25519.Pure as Donna
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SB8
import Control.Parallel.Strategies
-- import qualified Data.Either.Strict as Strict

import Control.Exception
import Data.Typeable
import Data.Maybe
import Control.DeepSeq


main :: IO ()
main = defaultMain
  [
--    bgroup "Simple Test" [
--      bench "sequential" $
--        whnf simpleTest0 signedMsgs
--    , bench "either error id" $
--        whnf simpleTest1 signedMsgs
--    , bench "Either String Int - rseq" $
--        whnf simpleTest2 signedMsgs
--    , bench "Either String Int - rdeepseq" $
--        whnf simpleTest3 signedMsgs
--    , bench "Strict.Either String Int - rseq" $
--        whnf simpleTest4 signedMsgs
--    , bench "Strict.Either String Int - rdeepseq" $
--        whnf simpleTest5 signedMsgs
--    , bench "IO mapConcurrently" $
--        whnfIO $ simpleTest6 signedMsgs
--    ]
--  bgroup "Chunking Test" [
--      bench "no chunking" $
--        whnf simpleTest2 signedMsgs
--    bench "chunk size = 2" $
--      whnf (chunkTest 2) signedMsgs
--    , bench "chunk size = 3" $
--        whnf (chunkTest 3) signedMsgs
--    , bench "chunk size = 4" $
--        whnf (chunkTest 4) signedMsgs
--    , bench "chunk size = 5" $
--        whnf (chunkTest 5) signedMsgs
--    , bench "chunk size = 6" $
--        whnf (chunkTest 6) signedMsgs
--    , bench "chunk size = 7" $
--        whnf (chunkTest 7) signedMsgs
--    , bench "chunk size = 8" $
--        whnf (chunkTest 8) signedMsgs
--    , bench "chunk size = 9" $
--        whnf (chunkTest 9) signedMsgs
--    , bench "chunk size = 10" $
--        whnf (chunkTest 10) signedMsgs
--    , bench "chunk size = 11" $
--        whnf (chunkTest 11) signedMsgs
--    , bench "chunk size = 12" $
--        whnf (chunkTest 12) signedMsgs
--    , bench "chunk size = 13" $
--        whnf (chunkTest 13) signedMsgs
--    , bench "chunk size = 14" $
--        whnf (chunkTest 14) signedMsgs
--    , bench "chunk size = 15" $
--        whnf (chunkTest 15) signedMsgs
--    , bench "chunk size = 16" $
--        whnf (chunkTest 16) signedMsgs
--    , bench "chunk size = 17" $
--        whnf (chunkTest 17) signedMsgs
--    , bench "chunk size = 18" $
--        whnf (chunkTest 18) signedMsgs
--    , bench "chunk size = 19" $
--        whnf (chunkTest 19) signedMsgs
--    , bench "chunk size = 20" $
--        whnf (chunkTest 20) signedMsgs
--    ]
    bgroup "Chunking Test - Donna" [
      bench "sequential" $
        whnf simpleTestDonna signedMsgsDonna
    , bench "no chunking" $
        whnf simpleTestDonna2 signedMsgsDonna
    , bench "chunk size = 2" $
        whnf (chunkTestDonna 2) signedMsgsDonna
    , bench "chunk size = 3" $
        whnf (chunkTestDonna 3) signedMsgsDonna
    , bench "chunk size = 4" $
        whnf (chunkTestDonna 4) signedMsgsDonna
    , bench "chunk size = 5" $
        whnf (chunkTestDonna 5) signedMsgsDonna
    , bench "chunk size = 6" $
        whnf (chunkTestDonna 6) signedMsgsDonna
    , bench "chunk size = 7" $
        whnf (chunkTestDonna 7) signedMsgsDonna
    , bench "chunk size = 8" $
        whnf (chunkTestDonna 8) signedMsgsDonna
    , bench "chunk size = 9" $
        whnf (chunkTestDonna 9) signedMsgsDonna
    , bench "chunk size = 10" $
        whnf (chunkTestDonna 10) signedMsgsDonna
    , bench "chunk size = 11" $
        whnf (chunkTestDonna 11) signedMsgsDonna
    , bench "chunk size = 12" $
        whnf (chunkTestDonna 12) signedMsgsDonna
    , bench "chunk size = 13" $
        whnf (chunkTestDonna 13) signedMsgsDonna
    , bench "chunk size = 14" $
        whnf (chunkTestDonna 14) signedMsgsDonna
    , bench "chunk size = 15" $
        whnf (chunkTestDonna 15) signedMsgsDonna
    , bench "chunk size = 16" $
        whnf (chunkTestDonna 16) signedMsgsDonna
    , bench "chunk size = 17" $
        whnf (chunkTestDonna 17) signedMsgsDonna
    , bench "chunk size = 18" $
        whnf (chunkTestDonna 18) signedMsgsDonna
    , bench "chunk size = 19" $
        whnf (chunkTestDonna 19) signedMsgsDonna
    , bench "chunk size = 20" $
        whnf (chunkTestDonna 20) signedMsgsDonna
    ]
--, bgroup "Signing Test" [
--    bench "sequential" $
--      whnf signTestSeq unsignedScript
--  , bench "no chunking" $
--      whnf signTestNoChunk unsignedScript
--  , bench "chunk = 5" $
--      whnf (signTestChunk 5) unsignedScript
--  , bench "chunk = 10" $
--      whnf (signTestChunk 10) unsignedScript
--  , bench "chunk = 25" $
--      whnf (signTestChunk 25) unsignedScript
--  , bench "chunk = 50" $
--      whnf (signTestChunk 50) unsignedScript
--  , bench "chunk = 100" $
--      whnf (signTestChunk 100) unsignedScript
--  ]
  , bgroup "Signing Test - Donna" [
      bench "sequential" $
        whnf signTestSeqDonna unsignedScript
    , bench "no chunking" $
        whnf signTestNoChunkDonna unsignedScript
    , bench "chunk = 5" $
        whnf (signTestChunkDonna 5) unsignedScript
    , bench "chunk = 10" $
        whnf (signTestChunkDonna 10) unsignedScript
    , bench "chunk = 25" $
        whnf (signTestChunkDonna 25) unsignedScript
    , bench "chunk = 50" $
        whnf (signTestChunkDonna 50) unsignedScript
    , bench "chunk = 100" $
        whnf (signTestChunkDonna 100) unsignedScript
    ]
  ]

data SimpleError = SimpleError String deriving (Eq, Show, Typeable)
instance Exception SimpleError

unsignedScript :: [ByteString]
unsignedScript = take 2000 (SB8.pack . show <$> ([1..] :: [Int]))

collectResults :: [Either String Int] -> Either String [Int]
collectResults ec = go ec []
  where
    go [] s = Right (reverse s)
    go ((Right cmd):cmds) s = go cmds (cmd:s)
    go ((Left err):_) _ = Left err

--privKey :: SecretKey
--privKey = SecretKey "\132h\138\225\233\237%\\\SOnZH\196\138\232\&7\239c'p)YE\192\136\DC3\217\170N\231n\236\199\NAK\238\171\\\161\222\247\186/\DC3\204Qqd\225}\202\150e~q\255;\223\233\211:\211\SUBT\145"
--
--pubKey :: PublicKey
--pubKey = toPublicKey privKey
--
--mySigned :: ByteString -> Int
--mySigned b = SB8.length $! unSignature $! dsign privKey b
--
--signedMsgs :: [(Signature, ByteString)]
--signedMsgs =  take 2000 ((\(i :: Int) -> (dsign privKey $ SB8.pack $ show i, SB8.pack $ show i)) <$> [1..])
--
--signTestSeq :: [ByteString] -> Int
--signTestSeq s = sum $! (mySigned <$> s)
--
--signTestNoChunk :: [ByteString] -> Int
--signTestNoChunk s = sum $! ((mySigned <$> s) `using` parList rdeepseq)
--
--signTestChunk :: Int -> [ByteString] -> Int
--signTestChunk n s = sum $! ((mySigned <$> s) `using` parListChunk n rdeepseq)
--
--instance NFData Signature
--
--verifySig :: PublicKey -> (Signature,ByteString) -> Int
--verifySig pk (s,b) = if dverify pk b s
--                     then read $! SB8.unpack b
--                     else error "Failed to verify!"
--
--verifySig' :: PublicKey -> (Signature,ByteString) -> Either String Int
--verifySig' pk (s,b) = if dverify pk b s
--                     then Right $! read $ SB8.unpack b
--                     else Left $! "Invalid Sig for: " ++ SB8.unpack b
--
-- verifySig'' :: PublicKey -> (Signature,ByteString) -> Strict.Either String Int
-- verifySig'' pk (s,b) = if dverify pk b s
--                      then Strict.Right $ read $ SB8.unpack b
--                      else Strict.Left $ "Invalid Sig for: " ++ SB8.unpack b
--
--verifySigExcept :: PublicKey -> (Signature,ByteString) -> Int
--verifySigExcept pk (s,b) = if dverify pk b s
--                     then read $ SB8.unpack b
--                     else throw $ SimpleError "InvalidSig"
--
-- collectResults' :: [Strict.Either String Int] -> Strict.Either String [Int]
-- collectResults' ec = go ec []
--   where
--     go [] s = Strict.Right (reverse s)
--     go ((Strict.Right cmd):cmds) s = go cmds (cmd:s)
--     go ((Strict.Left err):_) _ = Strict.Left err

---- ~1.4 sec
--simpleTest0 :: [(Signature,ByteString)] -> Int
--simpleTest0 s = sum ((either error id . verifySig' pubKey) <$> s)
--
---- ~540 milliseconds
--simpleTest1 :: [(Signature,ByteString)] -> Int
--simpleTest1 s = sum ((verifySig pubKey) <$> s `using` parList rdeepseq)
--
---- ~540 milliseconds
--simpleTest2 :: [(Signature,ByteString)] -> Int
--simpleTest2 s = either error sum $ collectResults ((verifySig' pubKey) <$> s `using` parList rseq)
--
---- ~550 milliseconds
--simpleTest3 :: [(Signature,ByteString)] -> Int
--simpleTest3 s = either error sum $ collectResults ((verifySig' pubKey) <$> s `using` parList rdeepseq)
--
-- ~550 milliseconds
-- simpleTest4 :: [(Signature,ByteString)] -> Int
-- simpleTest4 s = Strict.either error sum $ collectResults' ((verifySig'' pubKey) <$> s `using` parList rseq)

-- ~560 milliseconds
-- simpleTest5 :: [(Signature,ByteString)] -> Int
-- simpleTest5 s = Strict.either error sum $ collectResults' ((verifySig'' pubKey) <$> s `using` parList rdeepseq)

---- ~540 milliseconds
--simpleTest6 :: [(Signature,ByteString)] -> IO Int
--simpleTest6 s = do
--  ints <- mapConcurrently (\v -> return $! verifySigExcept pubKey v) s
--  return $ sum ints
--
--chunkTest :: Int -> [(Signature,ByteString)] -> Int
--chunkTest n s = either error sum $ collectResults ((verifySig' pubKey) <$> s `using` parListChunk n rseq)
--
privKeyDonna :: Donna.PrivateKey
privKeyDonna = fromJust $ Donna.importPrivate "\166\ESC\DC4\195\&3Pn\137\140uyV\n\v\DC4\225\143\172@\138\151\189\229\FS8\208A\NUL,\254m\179"

pubKeyDonna :: Donna.PublicKey
pubKeyDonna = fromJust $ Donna.importPublic "\228\159L\239\STX@\ACK\249\DLE\131\RSW\240\138W&\178\131\155\DLE\254\165\\:\238\EM\DC3\179o\EOTu\149"

mySignedDonna :: ByteString -> Int
mySignedDonna b = SB8.length $! (\(Donna.Sig b') -> b') $ Donna.sign b privKeyDonna pubKeyDonna

signedMsgsDonna :: [(Donna.Signature, ByteString)]
signedMsgsDonna =  take 2000 ((\(i :: Int) -> (Donna.sign (SB8.pack $ show i) privKeyDonna pubKeyDonna, SB8.pack $ show i)) <$> [1..])

signTestSeqDonna :: [ByteString] -> Int
signTestSeqDonna s = sum $! (mySignedDonna <$> s)

signTestNoChunkDonna :: [ByteString] -> Int
signTestNoChunkDonna s = sum $! ((mySignedDonna <$> s) `using` parList rdeepseq)

signTestChunkDonna :: Int -> [ByteString] -> Int
signTestChunkDonna n s = sum $! ((mySignedDonna <$> s) `using` parListChunk n rdeepseq)

instance NFData Donna.Signature where
  rnf (Donna.Sig b) = rnf b

verifySigDonna :: Donna.PublicKey -> (Donna.Signature,ByteString) -> Int
verifySigDonna pk (s,b) = if Donna.valid b pk s
                     then read $! SB8.unpack b
                     else error "Failed to verify!"

verifySigDonna' :: Donna.PublicKey -> (Donna.Signature,ByteString) -> Either String Int
verifySigDonna' pk (s,b) = if Donna.valid b pk s
                     then Right $! read $ SB8.unpack b
                     else Left $! "Invalid Sig for: " ++ SB8.unpack b

verifySigExceptDonna :: Donna.PublicKey -> (Donna.Signature,ByteString) -> Int
verifySigExceptDonna pk (s,b) = if Donna.valid b pk s
                     then read $ SB8.unpack b
                     else throw $ SimpleError "InvalidSig"

simpleTestDonna :: [(Donna.Signature,ByteString)] -> Int
simpleTestDonna s = either error sum $ collectResults ((verifySigDonna' pubKeyDonna) <$> s)

simpleTestDonna2 :: [(Donna.Signature,ByteString)] -> Int
simpleTestDonna2 s = either error sum $ collectResults ((verifySigDonna' pubKeyDonna) <$> s `using` parList rseq)

chunkTestDonna :: Int -> [(Donna.Signature,ByteString)] -> Int
chunkTestDonna n s = either error sum $ collectResults ((verifySigDonna' pubKeyDonna) <$> s `using` parListChunk n rseq)

-- Script Size = 10k
--benchmarking Simple Test/sequential
--time                 1.336 s    (1.314 s .. 1.360 s)
--                     1.000 R²   (1.000 R² .. 1.000 R²)
--mean                 1.329 s    (1.324 s .. 1.332 s)
--std dev              4.455 ms   (0.0 s .. 5.136 ms)
--variance introduced by outliers: 19% (moderately inflated)
--
--benchmarking Simple Test/either error id
--time                 523.8 ms   (507.5 ms .. 545.5 ms)
--                     1.000 R²   (0.999 R² .. 1.000 R²)
--mean                 523.7 ms   (519.7 ms .. 526.6 ms)
--std dev              4.323 ms   (0.0 s .. 4.955 ms)
--variance introduced by outliers: 19% (moderately inflated)
--
--benchmarking Simple Test/Either String Int - rseq
--time                 538.3 ms   (497.2 ms .. 633.7 ms)
--                     0.996 R²   (0.994 R² .. 1.000 R²)
--mean                 544.2 ms   (530.1 ms .. 552.9 ms)
--std dev              13.11 ms   (0.0 s .. 14.96 ms)
--variance introduced by outliers: 19% (moderately inflated)
--
--benchmarking Simple Test/Either String Int - rdeepseq
--time                 546.4 ms   (523.6 ms .. 569.4 ms)
--                     1.000 R²   (1.000 R² .. 1.000 R²)
--mean                 534.2 ms   (526.9 ms .. 539.4 ms)
--std dev              7.765 ms   (0.0 s .. 8.925 ms)
--variance introduced by outliers: 19% (moderately inflated)
--
--benchmarking Simple Test/Strict.Either String Int - rseq
--time                 525.4 ms   (494.1 ms .. 559.6 ms)
--                     0.999 R²   (0.998 R² .. 1.000 R²)
--mean                 546.9 ms   (538.1 ms .. 555.1 ms)
--std dev              13.75 ms   (0.0 s .. 14.21 ms)
--variance introduced by outliers: 19% (moderately inflated)
--
--benchmarking Simple Test/Strict.Either String Int - rdeepseq
--time                 545.1 ms   (494.8 ms .. 597.5 ms)
--                     0.999 R²   (0.996 R² .. 1.000 R²)
--mean                 546.6 ms   (538.9 ms .. 553.1 ms)
--std dev              10.40 ms   (0.0 s .. 11.28 ms)
--variance introduced by outliers: 19% (moderately inflated)
--

-- Script size = 2k
--benchmarking Chunking Test/no chunking
--time                 95.97 ms   (93.06 ms .. 98.49 ms)
--                     0.999 R²   (0.998 R² .. 1.000 R²)
--mean                 94.04 ms   (92.78 ms .. 95.33 ms)
--std dev              1.774 ms   (1.334 ms .. 2.293 ms)
--
--benchmarking Chunking Test/chunk size = 2
--time                 75.93 ms   (74.84 ms .. 76.88 ms)
--                     1.000 R²   (0.999 R² .. 1.000 R²)
--mean                 73.39 ms   (72.00 ms .. 74.24 ms)
--std dev              1.735 ms   (1.171 ms .. 2.596 ms)
--
--benchmarking Chunking Test/chunk size = 3
--time                 78.91 ms   (77.69 ms .. 80.32 ms)
--                     0.999 R²   (0.998 R² .. 1.000 R²)
--mean                 77.53 ms   (76.39 ms .. 78.40 ms)
--std dev              1.665 ms   (1.030 ms .. 2.160 ms)
--
--benchmarking Chunking Test/chunk size = 4
--time                 76.47 ms   (74.38 ms .. 78.93 ms)
--                     0.999 R²   (0.997 R² .. 1.000 R²)
--mean                 77.69 ms   (76.92 ms .. 78.64 ms)
--std dev              1.386 ms   (984.7 μs .. 1.908 ms)
--
--benchmarking Chunking Test/chunk size = 5
--time                 78.55 ms   (77.32 ms .. 79.71 ms)
--                     0.999 R²   (0.998 R² .. 1.000 R²)
--mean                 78.28 ms   (77.77 ms .. 78.98 ms)
--std dev              971.7 μs   (650.6 μs .. 1.330 ms)
--
--benchmarking Chunking Test/chunk size = 6
--time                 78.03 ms   (76.65 ms .. 79.63 ms)
--                     0.999 R²   (0.997 R² .. 1.000 R²)
--mean                 78.15 ms   (77.31 ms .. 79.67 ms)
--std dev              1.784 ms   (575.0 μs .. 2.574 ms)
--
--benchmarking Chunking Test/chunk size = 7
--time                 78.10 ms   (76.09 ms .. 80.97 ms)
--                     0.998 R²   (0.996 R² .. 1.000 R²)
--mean                 79.58 ms   (78.53 ms .. 80.49 ms)
--std dev              1.532 ms   (936.0 μs .. 2.199 ms)
--
--benchmarking Chunking Test/chunk size = 8
--time                 81.34 ms   (79.55 ms .. 83.22 ms)
--                     0.999 R²   (0.998 R² .. 1.000 R²)
--mean                 78.81 ms   (77.77 ms .. 79.85 ms)
--std dev              1.807 ms   (1.395 ms .. 2.301 ms)
--
--benchmarking Chunking Test/chunk size = 9
--time                 79.41 ms   (77.77 ms .. 80.68 ms)
--                     0.999 R²   (0.998 R² .. 1.000 R²)
--mean                 78.54 ms   (77.77 ms .. 79.58 ms)
--std dev              1.491 ms   (1.003 ms .. 2.166 ms)
--
--benchmarking Chunking Test/chunk size = 10
--time                 77.70 ms   (75.82 ms .. 80.03 ms)
--                     0.999 R²   (0.997 R² .. 1.000 R²)
--mean                 79.82 ms   (78.93 ms .. 80.67 ms)
--std dev              1.467 ms   (1.038 ms .. 2.151 ms)
--
--benchmarking Chunking Test/chunk size = 11
--time                 81.11 ms   (79.33 ms .. 82.20 ms)
--                     0.999 R²   (0.999 R² .. 1.000 R²)
--mean                 80.10 ms   (79.16 ms .. 80.99 ms)
--std dev              1.505 ms   (1.046 ms .. 2.294 ms)
--
--benchmarking Chunking Test/chunk size = 12
--time                 77.32 ms   (75.88 ms .. 78.28 ms)
--                     1.000 R²   (0.999 R² .. 1.000 R²)
--mean                 79.20 ms   (78.30 ms .. 81.97 ms)
--std dev              2.243 ms   (593.9 μs .. 3.550 ms)
--
--benchmarking Chunking Test/chunk size = 13
--time                 80.27 ms   (78.31 ms .. 81.81 ms)
--                     0.999 R²   (0.995 R² .. 1.000 R²)
--mean                 80.61 ms   (79.90 ms .. 82.48 ms)
--std dev              1.752 ms   (535.0 μs .. 2.747 ms)
--
--benchmarking Chunking Test/chunk size = 14
--time                 78.56 ms   (76.65 ms .. 81.04 ms)
--                     0.999 R²   (0.998 R² .. 1.000 R²)
--mean                 80.29 ms   (79.44 ms .. 81.75 ms)
--std dev              1.805 ms   (762.4 μs .. 3.066 ms)
--
--benchmarking Chunking Test/chunk size = 15
--time                 81.37 ms   (80.41 ms .. 82.42 ms)
--                     1.000 R²   (0.999 R² .. 1.000 R²)
--mean                 79.87 ms   (78.96 ms .. 80.67 ms)
--std dev              1.392 ms   (853.3 μs .. 1.858 ms)
--
--benchmarking Chunking Test/chunk size = 16
--time                 78.90 ms   (76.47 ms .. 80.91 ms)
--                     0.999 R²   (0.997 R² .. 1.000 R²)
--mean                 80.36 ms   (79.56 ms .. 81.29 ms)
--std dev              1.422 ms   (747.1 μs .. 2.509 ms)
--
--benchmarking Chunking Test/chunk size = 17
--time                 80.62 ms   (78.70 ms .. 82.78 ms)
--                     0.999 R²   (0.998 R² .. 1.000 R²)
--mean                 81.52 ms   (80.63 ms .. 83.19 ms)
--std dev              1.944 ms   (854.5 μs .. 3.066 ms)
--
--benchmarking Chunking Test/chunk size = 18
--time                 81.68 ms   (79.73 ms .. 83.35 ms)
--                     0.999 R²   (0.997 R² .. 1.000 R²)
--mean                 80.34 ms   (79.66 ms .. 81.33 ms)
--std dev              1.437 ms   (1.062 ms .. 1.861 ms)
--
--benchmarking Chunking Test/chunk size = 19
--time                 80.86 ms   (79.67 ms .. 82.11 ms)
--                     0.999 R²   (0.999 R² .. 1.000 R²)
--mean                 81.40 ms   (80.84 ms .. 82.65 ms)
--std dev              1.411 ms   (702.6 μs .. 2.131 ms)
--
--benchmarking Chunking Test/chunk size = 20
--time                 79.27 ms   (77.34 ms .. 82.00 ms)
--                     0.999 R²   (0.997 R² .. 1.000 R²)
--mean                 80.18 ms   (79.48 ms .. 80.99 ms)
--std dev              1.219 ms   (921.5 μs .. 1.591 ms)
