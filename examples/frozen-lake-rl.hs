{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
--
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

import CommonArgs
-- import Torch.Typed

import Control.Arrow ((&&&), (>>>))
import Control.Lens
import Data.Bifunctor (Bifunctor (bimap))
import Data.Finite (Finite)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Functor
import Data.Maybe (fromJust)
import GHC.Generics
import GHC.IsList
import GHC.TypeLits
import RL.Environment
import RL.Environments.FrozenLake
import RL.Environments.FrozenLake qualified as FL
import System.Random.Stateful hiding (Finite)
import Torch qualified as UT
import Torch.Initializers qualified as UT
import Torch.Internal.Managed.Type.Context
import Torch.Optim (foldLoop)
import Torch.Typed qualified as T
import Torch.Typed.Extra qualified as T

type Device = '(T.CUDA, 0)

type N = 8

main :: IO ()
main = do
  CommonArgs {learningRate, epoch, seed} <- getArgs
  mapM_ manual_seed_L seed
  g <- maybe newStdGen (pure . mkStdGen . fromIntegral) seed

  let (state0 :: FL.FrozenLake N, _) = uniform g
  putStrLn "Map:"
  print state0

  model0 <- T.sample (ModelSpec :: ModelSpec N)

  let maxStepCount = 2 * T.natValI @N ^ 2
  let optim0 = T.mkAdam 0 0.9 0.999 (T.flattenParameters model0)

  putStrLn "Training..."
  (model, _, _) <- foldLoop (model0, optim0, 0) epoch $ \(model, optim, winCount) i -> do
    episode <- runAgent (WithMaxStep {env = state0, maxStep = maxStepCount}) $ chooseAction model . snd

    let (rewards, logProbs) = unzip $ (\(_, (_, obs), logProb) -> (if obs == FL.goalPosition then 1 else 0 :: Float, logProb)) <$> episode
        logProbs' = UT.stack (UT.Dim 0) (T.toDynamic <$> logProbs)
        rewards' = UT.toDevice (UT.device logProbs') $ UT.asTensor $ scanr1 ((+) >>> (. (gamma *))) rewards
        gamma = 0.99

        loss = T.UnsafeMkTensor $ UT.sumAll $ rewards' * logProbs' :: T.Tensor Device T.Float '[]

    (model', optim') <- T.runStep model optim (-loss) (realToFrac learningRate)
    let successCount' = if last rewards == 1 then winCount + 1 else winCount

    let reportInterval = 100
    successCount'' <-
      if i `mod` reportInterval == 0
        then putStrLn ("Win rate: " <> show (fromIntegral successCount' / fromIntegral reportInterval)) $> 0
        else pure successCount'

    pure (model', optim', successCount'')

  putStrLn "Testing..."
  for_ [0 .. 10] $ \i -> do
    episode <- runAgent (WithMaxStep {env = state0, maxStep = maxStepCount}) $ chooseAction model . snd
    let (_, (_, lastPos), _) = last episode
    putStrLn $ "Episode " <> show i <> ": " <> if lastPos == FL.goalPosition then "Success" else "Fail" <> " " <> show (episode ^.. each . _1)

data Model (n :: Nat) = Model
  { layer1 :: T.Linear (n * n) (n * n * 2) T.Float Device,
    layer2 :: T.Linear (n * n * 2) (n * n) T.Float Device,
    layer3 :: T.Linear (n * n) 4 T.Float Device
  }
  deriving (Generic)

instance
  (KnownNat inputFeatures, KnownNat outputFeatures, T.KnownDType dtype, T.KnownDevice device) =>
  T.Randomizable (T.LinearSpec' inputFeatures outputFeatures dtype device) (T.Linear inputFeatures outputFeatures dtype device)
  where
  sample (T.KamimingUniform {..}) = do
    weight <- T.makeIndependent . T.UnsafeMkTensor . UT.toDevice (T.deviceVal @device) =<< UT.kaimingUniform fanMode nonLinearity [T.natValI @outputFeatures, T.natValI @inputFeatures]
    bias <- T.makeIndependent (T.zeros @'[outputFeatures] @dtype @device)
    pure $ T.Linear {..}

data ModelSpec (n :: Nat) = ModelSpec

instance (KnownNat (n * n), KnownNat (n * n * 2)) => T.Randomizable (ModelSpec n) (Model n) where
  sample _ = do
    layer1 <- T.sample (T.KamimingUniform UT.FanIn UT.Relu :: T.LinearSpec' (n * n) (n * n * 2) T.Float Device)
    layer2 <- T.sample (T.KamimingUniform UT.FanIn UT.Relu :: T.LinearSpec' (n * n * 2) (n * n) T.Float Device)
    layer3 <- T.sample (T.KamimingUniform UT.FanIn UT.Relu :: T.LinearSpec' (n * n) 4 T.Float Device)
    pure Model {..}

instance T.Parameterized (Model n)

chooseAction ::
  forall n.
  (KnownNat n, KnownNat (n * n)) =>
  Model n ->
  (Finite n, Finite n) ->
  IO (FL.Action, T.Tensor Device T.Float '[])
chooseAction Model {..} pos = do
  let probs :: T.Tensor Device T.Float '[4] =
        T.linearForward layer1 (T.reshape @'[_] input)
          & T.relu
          & T.linearForward layer2
          & T.relu
          & T.linearForward layer3
          & T.softmax @0

  (toEnum &&& (T.log . T.selectIdx @0 probs . fromIntegral)) . T.toInt <$> T.multinomial @1 probs
  where
    n = T.natValI @n
    (r, c) = bimap fromIntegral fromIntegral pos
    input :: T.Tensor Device T.Float '[n, n] =
      T.reshape @_ @'[n, n]
        . T.toDType @T.Float @T.Bool
        . T.UnsafeMkTensor
        $ UT.oneHot
          (n * n)
          (T.toDynamic (fromJust $ fromList [r * n + c :: Int] :: T.Tensor Device T.Int64 '[1]))
