module Reflex.Vty.Utils (withCtrlC, fillBoard, center) where

import Control.Monad.Fix
import Data.Functor
import Data.Text qualified as T
import Data.Traversable
import Graphics.Vty qualified as V
import Reflex
import Reflex.Network
import Reflex.Vty

withCtrlC :: (Monad m, HasInput t m, Reflex t) => m () -> m (Event t ())
withCtrlC f = do
  inp <- input
  f
  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing

fillBoard :: (Adjustable t m, NotReady t m, PostBuild t m, HasLayout t m, HasDisplayRegion t m, MonadFix m, HasInput t m, HasImageWriter t m, HasFocusReader t m, MonadHold t m, HasTheme t m) => ((Int, Int) -> Behavior t (V.Attr, Char)) -> m ()
fillBoard f = do
  dDimension <- zipDyn <$> displayWidth <*> displayHeight
  void $ networkView $ ffor dDimension \(w, h) -> do
    col $
      for [0 .. h - 1] $ \r ->
        grout (fixed 1) . row $
          for [0 .. w - 1] $ \c -> do
            let b = f (r, c)
            let bTextConfig = RichTextConfig $ fst <$> b
            let bText = T.singleton . snd <$> b
            grout (fixed 1) $
              richText bTextConfig bText

center :: (Adjustable t m, NotReady t m, PostBuild t m, HasLayout t m, MonadFix m, MonadHold t m, HasDisplayRegion t m, HasInput t m, HasImageWriter t m, HasFocusReader t m) => m a -> m a
center m =
  grout flex . col $
    grout flex (pure ()) *> m <* grout flex (pure ())
