{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Tardis.Instances where

import Control.Monad.Fix
import Control.Monad.Tardis
import Control.Monad.Tardis.Class
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Tardis (TardisT (..))
import Control.Monad.Trans.Writer
import System.IO (fixIO)

instance (MonadTardis bw fw m) => MonadTardis bw fw (MaybeT m) where
  getPast = lift getPast
  sendPast = lift . sendPast
  getFuture = lift getFuture
  sendFuture = lift . sendFuture

instance (MonadTardis bw fw m, Monoid w) => MonadTardis bw fw (WriterT w m) where
  getPast = lift getPast
  sendPast = lift . sendPast
  getFuture = lift getFuture
  sendFuture = lift . sendFuture

instance (MonadFix m, MonadIO m) => MonadIO (TardisT bw fw m) where
  liftIO m = TardisT $ \s -> liftIO m >>= \a -> pure (a, s)

-- instance (MonadFix m, MonadIO m) => MonadIO (BackwardStateT s m) where
