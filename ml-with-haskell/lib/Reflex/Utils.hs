module Reflex.Utils where

import Control.Concurrent
import Control.Concurrent.Thread.Delay qualified as Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Time
import Reflex
import Reflex.Network

networkReset ::
  forall t m a.
  (Adjustable t m, MonadHold t m) =>
  m a ->
  Event t () ->
  m (Dynamic t a)
networkReset m e = networkHold m (e `ffor` const m)

-- | Delay an Event's occurrences by a given amount in seconds.
delay' :: (PerformEvent t m, TriggerEvent t m, MonadIO (Performable m)) => Behavior t (NominalDiffTime) -> Event t a -> m (Event t a)
delay' bdt e =
  performEventAsync $
    attach bdt e `ffor` \(dt, a) cb -> liftIO $ void $ forkIO $ do
      Concurrent.delay $ ceiling $ dt * 1000000
      cb a

newEventFromChan :: (MonadIO m, TriggerEvent t m) => Chan a -> m (Event t a)
newEventFromChan ch = do
  newEventWithLazyTriggerWithOnComplete $ \trigger -> do
    threadId <- liftIO . forkIO . forever $ readChan ch >>= flip trigger (pure ())
    pure (killThread threadId)
