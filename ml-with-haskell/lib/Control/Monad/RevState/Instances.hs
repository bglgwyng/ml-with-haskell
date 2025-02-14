{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.RevState.Instances where

import Control.Monad.Except (ExceptT)
import Control.Monad.Fix
import Control.Monad.RevState hiding (StateT)
import Control.Monad.RevState qualified as RS
import Control.Monad.State.Lazy qualified as LS
import Control.Monad.State.Strict qualified as SS
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer

instance (MonadRevState s m) => MonadRevState s (MaybeT m) where
  get = lift get
  put = lift . put

instance (MonadRevState s m, Monoid w) => MonadRevState s (WriterT w m) where
  get = lift get
  put = lift . put

instance (MonadRevState s m) => MonadRevState s (ExceptT e m) where
  get = lift get
  put = lift . put

instance (MonadRevState s m) => MonadRevState s (LS.StateT s' m) where
  get = lift get
  put = lift . put

instance (MonadRevState s m) => MonadRevState s (SS.StateT s' m) where
  get = lift get
  put = lift . put

instance (MonadFix m, MonadIO m) => MonadIO (RS.StateT s m) where
  liftIO m = RS.StateT $ \s -> liftIO m >>= \a -> pure (a, s)
