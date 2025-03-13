module Control.Arrow.TorchArrow (TorchArrow, runTorchArrow, cast, add, mul) where

import Control.Arrow
import Control.Arrow.Free
import Control.Category.Free (foldNatQ)
import Torch hiding (add, mul)

data Op a b where
  Op :: (a -> IO b) -> Op a b

type TorchArrow = Arr Op

runTorchArrow :: TorchArrow a b -> a -> IO b
runTorchArrow Id x = pure x
runTorchArrow (Cons (Op f) gs) x = do
  b <- runKleisli (foldNatQ (Kleisli . runTorchArrow) gs) x
  f b
runTorchArrow (Arr f g) x = f <$> runTorchArrow g x
runTorchArrow (Prod f g) x = do
  y1 <- runTorchArrow f x
  y2 <- runTorchArrow g x
  pure (y1, y2)

cast :: TorchArrow Tensor Tensor
cast = Cons (Op $ pure . toDevice (Device CPU 0)) mempty

add :: TorchArrow (Tensor, Tensor) Tensor
add = Cons (Op (\(x, y) -> pure $ x + y)) mempty

mul :: TorchArrow (Tensor, Tensor) Tensor
mul = Cons (Op (\(x, y) -> pure $ x * y)) mempty