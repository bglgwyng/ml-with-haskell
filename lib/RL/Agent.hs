module RL.Agent where

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe

class MonadAgent o a m | m -> o a where
  observe :: m o
  act :: a -> m ()
