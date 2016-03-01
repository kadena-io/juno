module Apps.Juno.Server
  ( main
  ) where

import Juno.Spec.Simple
import Juno.Runtime.Types (CommandEntry, CommandResult)
import Apps.Juno.Command

-- | Runs a 'Raft nt String String mt'.
main :: IO ()
main = do
  stateVariable <- starterEnv
  let -- applyFn :: et -> IO rt
      applyFn :: CommandEntry -> IO CommandResult
      applyFn = runCommand stateVariable
  runServer applyFn
