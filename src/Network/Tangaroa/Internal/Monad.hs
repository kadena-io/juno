{-# LANGUAGE TemplateHaskell #-}

module Network.Tangaroa.Internal.Monad
  ( Raft
  , RaftEnv(..), cfg, conn, eventIn, eventOut
  , fork
  , sendEvent
  ) where

import Control.Lens hiding (Index)
import Network.Tangaroa.Types

import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.Chan.Unagi
import Control.Monad.RWS

data RaftEnv nt mt ht = RaftEnv
  { _cfg :: Config nt
  , _conn :: ht
  , _eventIn :: InChan (Event mt)
  , _eventOut :: OutChan (Event mt)
  }
makeLenses ''RaftEnv

type Raft nt mt ht a = RWST (RaftEnv nt mt ht) () (VolatileState nt) IO a

fork :: IO () -> Raft nt mt ht ThreadId
fork = lift . forkIO

sendEvent :: Event mt -> Raft nt mt ht ()
sendEvent event = do
  ein <- view eventIn
  lift $ writeChan ein event
