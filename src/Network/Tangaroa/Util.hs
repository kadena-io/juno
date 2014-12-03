{-# LANGUAGE FlexibleContexts #-}

module Network.Tangaroa.Util
  ( seqIndex
  , lastLogInfo
  , debug
  , fork_
  , enqueueEvent
  , dequeueEvent
  ) where

import Network.Tangaroa.Types
import Control.Lens hiding (Index)
import Data.Sequence (Seq)
import Control.Monad.Trans (lift)
import Control.Concurrent.Lifted (fork)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Concurrent.Chan.Unagi
import qualified Data.Sequence as Seq

seqIndex :: Seq a -> Int -> Maybe a
seqIndex s i =
  if i >= 0 && i < Seq.length s
    then Just (Seq.index s i)
    else Nothing

-- get the last term and index of a log
lastLogInfo :: Seq (Term, et) -> (Term, Index)
lastLogInfo es =
  case Seq.viewr es of
    _ Seq.:> (t,_) -> (t, Seq.length es - 1)
    Seq.EmptyR     -> (startTerm, startIndex)

debug :: String -> Raft nt et rt mt ()
debug s = view (rs.debugPrint) >>= ($ s)

fork_ :: MonadBaseControl IO m => m () -> m ()
fork_ a = fork a >> return ()

enqueueEvent :: Event mt -> Raft nt et rt mt ()
enqueueEvent event = do
  ein <- view eventIn
  lift $ writeChan ein event

dequeueEvent :: Raft nt et rt mt (Event mt)
dequeueEvent = lift . readChan =<< view eventOut
