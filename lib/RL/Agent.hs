module RL.Agent where

class MonadAgent o a m | m -> o a where
  observe :: m o
  act :: a -> m ()
