{-# LANGUAGE UndecidableInstances #-}

import CommonArgs
import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.RevState qualified as RS
import Control.Monad.RevState.Instances ()
import Control.Monad.State
import Control.Monad.Tardis.Instances ()
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Writer
import Data.Finite (Finite)
import Data.Foldable (for_)
import Data.Functor
import Data.Maybe (fromJust)
import Data.Vector.Sized qualified as V
import GHC.Generics
import GHC.IsList
import GHC.TypeLits
import RL.Agent
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

  model0 <- T.sample (ModelSpec :: ModelSpec N)

  let (state0 :: FL.FrozenLake N, _) = uniform g
  putStrLn "Map:"
  print state0
  let maxStepCount = 2 * T.natValI @N ^ 2
  let optim0 = T.mkAdam 0 0.9 0.999 (T.flattenParameters model0)

  putStrLn "Training..."
  (model, _, _) <- foldLoop (model0, optim0, 0) epoch $ \(model, optim, winCount) i -> do
    let gamma = 0.99
    (hasWon, episode) <-
      flip RS.evalStateT (0 :: Float)
        . flip evalStateT state0
        . runWriterT
        . runExceptT
        $ for_ [1 .. maxStepCount] \_ -> do
          position <- observe

          when (FL.goalPosition == position) $ RS.put 1 *> throwE True
          when (state0.map `V.index` fst position `V.index` snd position == FL.H) $ RS.put 0 *> throwE False

          (action, prob) <- liftIO $ chooseAction model position
          act action

          reward <- RS.modify (* gamma) *> RS.get
          lift $ tell [(action, (prob, reward))]

    let (rewards, logProbs) = unzip $ (\(_, (logProb, reward)) -> (reward, logProb)) <$> episode
        logProbs' = UT.stack (UT.Dim 0) (T.toDynamic <$> logProbs)
        rewards' = UT.toDevice (UT.device logProbs') $ UT.asTensor rewards

        loss = T.UnsafeMkTensor $ UT.sumAll $ rewards' * logProbs' :: T.Tensor Device T.Float '[]

    (model', optim') <- T.runStep model optim (-loss) (realToFrac learningRate)
    let winCount' = if hasWon == Left True then winCount + 1 else winCount

    let reportInterval = 100
    winCount'' <-
      if i `mod` reportInterval == 0
        then putStrLn ("Win rate: " <> show (fromIntegral winCount' / fromIntegral reportInterval)) $> 0
        else pure winCount'

    pure (model', optim', winCount'')

  putStrLn "Testing..."
  for_ [0 .. 10] $ \i -> do
    win <- flip evalStateT state0
      . runExceptT @Bool
      $ do
        for_ [1 .. maxStepCount] \_ -> do
          position <- observe

          when (FL.goalPosition == position) $ throwE True
          when (state0.map `V.index` fst position `V.index` snd position == FL.H) $ throwE False

          act . fst =<< liftIO (chooseAction model position)
    putStrLn $ "Episode " <> show i <> ": " <> if win == Left True then "Success" else "Fail"

data Model (n :: Nat) = Model
  { layer1 :: T.Linear (n * n) (n * n * 2) T.Float Device,
    layer2 :: T.Linear (n * n * 2) (n * n) T.Float Device,
    layer3 :: T.Linear (n * n) 4 T.Float Device
  }
  deriving (Generic)

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
