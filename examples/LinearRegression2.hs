import Control.Monad.State
import Data.Foldable
import GHC.TypeNats
import Text.Printf
import Torch.Internal.Managed.Type.Context
import Torch.Typed hiding (Device)
import Torch.Typed qualified as T hiding (Device)
import Torch.Typed.Extra qualified as T

type Device = '(CUDA, 0)

type N = 100

main :: IO ()
main = do
  manual_seed_L 42

  let (wTrue, bTrue) = (5, -2)
  printf "wTrue: %f, bTrue: %f\n" wTrue bTrue
  (xs, ys) <- generate @N @Device wTrue bTrue 0.1

  let epoch = 100
  let learningRate = 0.5

  w0 :: Parameter Device 'Float '[] <- makeIndependent =<< T.rand
  b0 :: Parameter Device 'Float '[] <- makeIndependent =<< T.rand

  (wLearned, bLearned) <- flip execStateT (w0, b0) $ do
    for_ [1 .. epoch] $ \i -> do
      (p_w, p_b) <- get
      let w = toDependent p_w
      let b = toDependent p_b

      let predictions = (T.expand' w * xs) + T.expand' b

      let errs = predictions - ys
      let loss = T.meanAll $ T.powScalar (2 :: Float) errs

      liftIO $ printf "loss: %f %f\n" (toFloat loss) (toFloat $ T.sumAll errs)

      let w' = grad loss p_w
      let b' = grad loss p_b

      p_w <- liftIO $ makeIndependent $ w - w' * learningRate
      p_b <- liftIO $ makeIndependent $ b - b' * learningRate
      put (p_w, p_b)

      liftIO $ printf "epoch: %4d, loss: %0.5f\n" (i :: Int) (toFloat loss)

  liftIO $ printf "wLearned: %f, bLearned: %f\n" (toFloat (toDependent wLearned)) (toFloat (toDependent bLearned))

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

  let ys = T.addScalar b (T.mulScalar w xs) + errs

  pure (xs, ys)
