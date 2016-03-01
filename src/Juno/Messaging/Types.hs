{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Juno.Messaging.Types (
  Spec(..)
  ,Addr(..)
  ,OutBoundMsg(..)
  ,Recipients(..)
  ,ListenOn(..)
  ,Rolodex(..)
  ) where

import Control.Concurrent.Chan.Unagi
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Serialize
import GHC.Generics (Generic)

data Spec addr msg sock = Spec {
  -- | Messages for you
  _sInbox   :: Serialize msg => InChan msg
  -- | Messages that you want to send
  ,_sOutbox :: Serialize addr => OutChan (OutBoundMsg addr msg)
  -- | What the receiver listens on
  ,_sWhoAmI :: Addr addr
  ,_sRolodex :: Rolodex addr sock
  }

newtype Addr a = Addr { _unAddr :: a } deriving (Read,Show,Eq,Ord)
newtype Rolodex a s = Rolodex {_unRolodex :: Map.Map (Addr a) (ListenOn s)}
newtype ListenOn a = ListenOn {_unListenOn :: a}

-- | Specifiy who the message should go to
data Recipients a = RAll
                  | RSome (Set.Set (Addr a))
                  | ROne (Addr a)
                  deriving (Show,Eq,Generic)

data OutBoundMsg addr msg = OutBoundMsg {
  _obmTo     :: Recipients addr
  , _obmBody :: msg
  } deriving (Show, Eq)
