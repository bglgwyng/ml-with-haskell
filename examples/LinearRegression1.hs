import Control.Monad.State
import Data.Foldable
import GHC.TypeNats
import Text.Printf
import Torch.Typed hiding (Device)
import Torch.Typed qualified as T hiding (Device)
import Torch.Typed.Extra qualified as T

type Device = '(CUDA, 0)

type N = 100

main :: IO ()
main = do
  let n = natValI @N

  let (wTrue, bTrue) = (5, -2)
  printf "wTrue: %f, bTrue: %f\n" wTrue bTrue
  (xs, ys) <- generate @N @Device wTrue bTrue 0.1

  let epoch = 100
  let learningRate = 1

  w0 <- toFloat <$> T.rand @_ @_ @Device
  b0 <- toFloat <$> T.rand @_ @_ @Device

  (wLearned, bLearned) <- flip execStateT (w0, b0) $ do
    for_ [1 .. epoch] $ \i -> do
      (w, b) <- get

      let predictions = b `T.addScalar` (w `T.mulScalar` xs)
      let errs = predictions - ys

      let w' = toFloat $ T.meanAll ((w `T.mulScalar` xs - ys `T.addScalar'` b) * xs) `T.mulScalar'` (2 :: Float)
      let b' = toFloat $ T.meanAll errs

      let loss = toFloat $ T.divScalar n $ T.sumAll $ T.powScalar (2 :: Float) errs
      put (w - w' * learningRate, b - b' * learningRate)
      liftIO $ printf "epoch: %4d, loss: %0.5f\n" (i :: Int) loss

  liftIO $ printf "wLearned: %f, bLearned: %f\n" wLearned bLearned

generate ::
  forall n device.
  (T.RandDTypeIsValid device T.Float, KnownNat n, T.KnownDevice device) =>
  Float ->
  Float ->
  Float ->
  IO (T.Tensor device T.Float '[n], T.Tensor device T.Float '[n])
generate w b e = do
  xs :: T.Tensor device T.Float '[n] <- T.rand
  errs :: T.Tensor device T.Float '[n] <- T.mulScalar e . T.subScalar (1 :: Float) . T.mulScalar (2 :: Float) <$> T.rand
  print errs
  let ys = T.addScalar b (T.mulScalar w xs) + errs

  pure (xs, ys)
