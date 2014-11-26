{-# LANGUAGE TemplateHaskell #-}

module Network.Tangaroa.Internal.Monad
  ( Raft
  , RaftEnv(..), cfg, eventIn, eventOut
  ) where

import Control.Lens hiding (Index)
import Network.Tangaroa.Types

import Control.Concurrent.Chan.Unagi
import Control.Monad.RWS

data RaftEnv nt mt = RaftEnv
  { _cfg :: Config nt
  , _eventIn :: InChan (Event mt)
  , _eventOut :: OutChan (Event mt)
  }
makeLenses ''RaftEnv

type Raft nt mt = RWST (RaftEnv nt mt) () (VolatileState nt) IO ()

sendEvent :: Event mt -> Raft nt mt
sendEvent event = do
  ein <- view eventIn
  lift $ writeChan ein event
