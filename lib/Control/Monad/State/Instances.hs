{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.State.Instances where

import Control.Monad.Fix
import Control.Monad.RevState qualified as RS
import Control.Monad.State

-- instance (MonadState s m, MonadFix m) => MonadState s (RS.StateT s m) where
--   get = lift get
--   put = lift . put
