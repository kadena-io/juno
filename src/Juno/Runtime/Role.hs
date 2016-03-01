
module Juno.Runtime.Role
  ( becomeFollower
  ) where

import Control.Lens hiding (Index)

import Juno.Runtime.Timer
import Juno.Runtime.Types
import Juno.Util.Util



-- THREAD: unknown/unused. updates state
becomeFollower :: Monad m => Raft m ()
becomeFollower = do
  debug "becoming follower"
  role .= Follower
  resetElectionTimer
