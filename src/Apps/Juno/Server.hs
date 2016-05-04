module Apps.Juno.Server
  ( main
  ) where

import Control.Concurrent.Chan.Unagi

import Apps.Juno.Command
import Juno.Spec.Simple
import Juno.Types (CommandEntry, CommandResult, initCommandMap)
import Juno.Types.Message.CMD (Command(..))

-- | Runs a 'Raft nt String String mt'.
main :: IO ()
main = do
  stateVariable <- starterEnv
  (toCommands, fromCommands) <- newChan -- (RequestId, [CommandEntries])
  -- shared on a node basis between API interface and protocol
  sharedCmdStatusMap <- initCommandMap
  let -- applyFn :: et -> IO rt
      applyFn :: Command -> IO CommandResult
      applyFn = runCommand stateVariable
  runJuno applyFn toCommands fromCommands sharedCmdStatusMap
