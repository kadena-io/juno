module Network.Tangaroa.Internal.Monad
  ( Raft
  ) where

import Network.Tangaroa.Types

import Control.Concurrent.Lifted.Fork
import Control.Monad.RWS.Concurrent

type Raft nt = RWSC (Config nt) () (VolatileState nt) IO ()
