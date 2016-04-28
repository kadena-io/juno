{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Apps.Juno.Repl
 ( main
 ) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Char as C
import Control.Exception
import Data.Aeson as JSON
import Data.Either ()
import qualified Data.Text as T
import qualified Network.Wreq as W
import System.IO

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

submitRequest :: BLC.ByteString -> String -> IO (W.Response BLC.ByteString)
submitRequest reqBody relativePath = W.post (apiEndpoint 8001 ++ relativePath) reqBody

showResult :: Show a => a -> IO ()
showResult res = putStrLn $ promptGreen ++ show res

-- |
-- ObserveAccounts
-- CreateAccount Acct1
-- AdjustAccount Acct1 100.0
-- transfer(Acct1->Acct2, 1%1)
runREPL :: IO ()
runREPL = do
  input <- readPrompt
  case input of
    "" -> runREPL
    _ -> catch
         (processInput input)
         (\(e :: SomeException) -> putStrLn $ "Invalid Command: " ++ show e) >> runREPL
  where

    processInput :: String -> IO()
    processInput input =
      case requestInfo input of
        Right (body, relPath) -> do
            resp <- submitRequest body relPath
            showResult resp
        Left err -> showResult err

    requestInfo :: String -> Either String (BLC.ByteString, String)
    requestInfo input
       -- batch test: 500 transfer(Acct1->Acct2, 1 % 1)
       -- batch test: 500 AdjustAccount Acct1 2.0
      | isBatchTestCmd input = do
          let szAndCmd = T.strip . T.pack $ drop 11 input
          let ctText = T.takeWhile C.isNumber szAndCmd -- num of cmds to include in the batch
          let cmd = T.drop (T.length ctText) szAndCmd  -- cmd to batch
          let ct = read . T.unpack $ ctText
          case replCmd2json (T.unpack cmd) of
            Left err -> Left err
            Right cmdJson ->
              let cmds = replicate ct cmdJson
              in Right (batchReqJson $ JsonT.CommandBatch cmds, "api/juno/v1/cmd/batch")

      --  Poll rid1 rid2 rid3
      | isPollCmd input = do
          let rids = (tail . words) input -- drop "Poll" and get [rids] (tail)
          Right (pollRidsJson $ fmap T.pack rids, "api/juno/v1/poll")

      -- Single command, i.e. CreateAccount Acct1, AdjustAccount Acct1 10%1, or raw Program.
      -- REPL command -> json representation and submitted to API as a batch of one elem.
      | otherwise =
          case replCmd2json input of
            Left err -> Left err
            Right cmdJson ->
              Right (batchReqJson (JsonT.CommandBatch [cmdJson]), "api/juno/v1/cmd/batch")

    toText = T.pack . BLC.unpack

    replCmd2json input =
      case readHopper (BSC.pack input) of
        Left err -> Left $ input ++ " " ++ err
        Right cmd -> Right $ toText $ commandToJson cmd input

    commandToJson :: HopperLiteAdminCommand -> String -> BLC.ByteString
    commandToJson (CreateAccount acct) _ = JSON.encode $ JsonT.AccountPayload (T.strip acct)
    commandToJson (AdjustAccount acct amount) _ =
      JSON.encode $ JsonT.AccountAdjustPayload acct (JsonT.JRational amount)
    commandToJson _ input = JSON.encode $ JsonT.TransactBody (T.pack input) ""

    batchToken :: String
    batchToken = "batch test:"

    isBatchTestCmd :: String -> Bool
    isBatchTestCmd input = take 11 input == batchToken

    isPollCmd :: String -> Bool
    isPollCmd input = take 4 input == "Poll"

    batchReqJson :: JsonT.CommandBatch -> BLC.ByteString
    batchReqJson cmdBatch = JSON.encode $
                              JsonT.CommandBatchRequest
                                cmdBatch
                                (JsonT.Digest "hashy" "mykey")

    pollRidsJson :: [T.Text] -> BLC.ByteString
    pollRidsJson rids = JSON.encode $
                          JsonT.PollPayloadRequest
                           (JsonT.PollPayload rids)
                           (JsonT.Digest "hashy" "mykey")

-- | Runs a 'Raft nt String String mt'.
-- Simple fixes nt to 'HostPort' and mt to 'String'.
main :: IO ()
main = runREPL -- TODO: REPL will be responsible for singing requests.
