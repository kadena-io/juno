
module Juno.Runtime.Role
  ( becomeFollower
  ) where

import Juno.Runtime.Timer
import Juno.Runtime.Types
import Juno.Runtime.Protocol.Types
import Juno.Util.Util

-- THREAD: unknown/unused. updates state
becomeFollower :: Monad m => Raft m ()
becomeFollower = do
  debug "becoming follower"
  setRole Follower
  resetElectionTimer
