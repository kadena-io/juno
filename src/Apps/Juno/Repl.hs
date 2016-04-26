{-# LANGUAGE OverloadedStrings #-}
module Apps.Juno.Repl
 ( main
 ) where

import Control.Concurrent.Lifted (threadDelay)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Either ()
import Data.Aeson as JSON
import qualified Data.Text as T
import qualified Network.Wreq as W
import System.IO

import Juno.Types

import Apps.Juno.Parser
import qualified Apps.Juno.JsonTypes as JsonT

prompt :: String
prompt = "\ESC[0;31mhopper>> \ESC[0m"

promptGreen :: String
promptGreen = "\ESC[0;32mresult>> \ESC[0m"

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: IO String
readPrompt = flushStr prompt >> getLine

apiEndpoint :: Int -> String
apiEndpoint port = "http://localhost:" ++ show port ++ "/"

submitCmdBatch :: BLC.ByteString -> IO (W.Response BLC.ByteString)
submitCmdBatch cmdBatch = W.post (cmdBathURL 8001) cmdBatch
 where
   cmdBathURL p = apiEndpoint p ++ "api/juno/v1/cmd/batch"

-- { "payload": {"cmdids": ["rid_1","rid_2"]},"digest": { "hash": "string", "key": "string" } }
submitPollRequestId :: BLC.ByteString -> IO (W.Response BLC.ByteString)
submitPollRequestId pollReq = W.post (pollURL 8001) pollReq
 where
   pollURL p = apiEndpoint p ++ "api/juno/v1/poll"

showResult :: Show a => a -> IO ()
showResult res = putStrLn $ promptGreen ++ show res

-- |
-- ObserveAccounts
-- CreateAccount Acct1
-- AdjustAccount Acct1 1.0
-- transfer(Acct1->Acct2, 1%1)
runREPL :: IO ()
runREPL = do
  cmd <- readPrompt
  case cmd of
    "" -> runREPL
    _ -> processInput cmd >> runREPL
  where
    processInput input = do
      cmd' <- return $ BSC.pack input
      if take 11 input == "batch test:"
      then do
        showResult $ show [CommandEntry cmd']
        threadDelay 1000
      else if take 4 input == "Poll"
      then do
        let cmds = (tail . words) input
        let pollRequestsJson = rids2PollJSON $ fmap T.pack cmds
        res <- submitPollRequestId pollRequestsJson
        showResult res
      else
        case readHopper cmd' of
          Left err -> putStrLn input >> putStrLn err >> runREPL
          Right _ -> do
             let cmdJsonBytes = T.pack . BLC.unpack $ JsonT.commandToJSONBytes cmd'
             let jsonBytes = cmdBatch2JSON (JsonT.CommandBatch [cmdJsonBytes])
             print jsonBytes
             res <- submitCmdBatch jsonBytes
             showResult res

    cmdBatch2JSON :: JsonT.CommandBatch -> BLC.ByteString
    cmdBatch2JSON cmdBatch = JSON.encode $
                              JsonT.CommandBatchRequest
                                cmdBatch
                                (JsonT.Digest "hashy" "mykey")

    rids2PollJSON :: [T.Text] -> BLC.ByteString
    rids2PollJSON rids = JSON.encode $
                          JsonT.PollPayloadRequest
                           (JsonT.PollPayload rids)
                           (JsonT.Digest "hashy" "mykey")

-- | Runs a 'Raft nt String String mt'.
-- Simple fixes nt to 'HostPort' and mt to 'String'.
main :: IO ()
main = runREPL -- TODO: REPL will be responsible for singing requests.
