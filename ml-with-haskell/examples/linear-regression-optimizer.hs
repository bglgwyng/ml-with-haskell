{-# LANGUAGE OverloadedLists #-}

import CommonArgs
import Control.Monad (when)
import Data.Maybe (fromJust)
import Torch.Internal.Managed.Type.Context
import Torch.Optim (foldLoop)
import Torch.Typed hiding (Device)
import Torch.Typed qualified as T hiding (Device)
import Torch.Typed.Extra qualified as T

type Device = '(CUDA, 0)

type BatchSize = 100

type InputFeatures = 3

type OutputFeatures = 1

groundTruth :: Tensor Device T.Float '[BatchSize, InputFeatures] -> Tensor Device T.Float '[BatchSize, OutputFeatures]
groundTruth t = t `matmul` weight + T.expand' bias
  where
    weight = fromJust [[42.0], [64.0], [96.0]] :: Tensor Device T.Float '[InputFeatures, OutputFeatures]
    bias = fromJust [1.0] :: Tensor Device T.Float '[OutputFeatures]

model ::
  Linear InputFeatures OutputFeatures T.Float Device ->
  Tensor Device T.Float '[BatchSize, InputFeatures] ->
  Tensor Device T.Float '[BatchSize, OutputFeatures]
model state = linear (toDependent state.weight) (toDependent state.bias)

main :: IO ()
main = do
  CommonArgs {learningRate, epoch, seed} <- getArgs
  mapM_ manual_seed_L seed

  init <- sample LinearSpec

  trained <- foldLoop init epoch $ \state i -> do
    input <- randn @'[BatchSize, InputFeatures] @T.Float @'(CUDA, 0)
    let (ys, ys') = (groundTruth input, model state input)
    let loss = mseLoss @ReduceMean ys ys'

    when (i `mod` 100 == 0) $ do
      putStrLn $ "Iteration: " <> show i <> " | Loss: " <> show loss

    (state', _) <- runStep state GD loss (realToFrac learningRate)
    pure state'

  putStrLn $ "wLearned: " <> show (squeezeAll $ toDependent trained.weight)
  putStrLn $ "bLearned: " <> show (squeezeAll $ toDependent trained.bias)
