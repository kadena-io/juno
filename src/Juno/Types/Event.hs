
module Juno.Types.Event
  ( Event(..)
  ) where

import Juno.Types.Message

data Event = ERPC RPC
           | AERs AlotOfAERs
           | ElectionTimeout String
           | HeartbeatTimeout String
  deriving (Show)
