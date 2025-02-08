{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

import CommonArgs
import Control.Arrow hiding (app)
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.RevState.Instances ()
import Control.Monad.State
import Control.Monad.Tardis
import Control.Monad.Tardis.Instances ()
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Bifunctor hiding (first, second)
import Data.Finite
import Data.Foldable (for_)
import Data.Function
import Data.Functor
import Data.Maybe hiding (catMaybes, mapMaybe)
import Data.Monoid (Endo (Endo, appEndo))
import Data.Set qualified as Set
import Data.Traversable (for)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as V
import Data.Witherable
import GHC.Generics
import GHC.IsList
import GHC.TypeNats
import Graphics.Vty qualified as V
import Grid
import RL.Environments.Snake
import RL.Environments.Snake qualified as S
import Reflex
import Reflex.Network
import Reflex.Utils
import Reflex.Vty
import System.Exit (exitSuccess)
import System.IO (hPrint, stderr)
import System.Random hiding (Finite)
import Torch qualified as UT
import Torch.Data.CsvDatastream (CsvDatastream' (batchSize))
import Torch.HList
import Torch.Initializers qualified as UT
import Torch.Internal.Managed.Type.Context
import Torch.Optim (foldLoop)
import Torch.Typed qualified as T
import Torch.Typed.Extra qualified as T
import Torch.Typed.VLTensor qualified as VL

type Device = '(T.CUDA, 0)

type N = 8

n :: Int
n = T.natValI @N

main :: IO ()
main = do
  CommonArgs {learningRate, epoch, seed, loadPath, savePath} <- getArgs
  mapM_ manual_seed_L seed
  g <- maybe newStdGen (pure . mkStdGen . fromIntegral) seed

  chReq :: Chan (Chan Grid.Direction) <- newChan
  chResp :: Chan (Chan (Maybe (S.Observation N))) <- newChan

  model0 <- case loadPath of
    Just path -> do
      model <- T.sample (ModelSpec @N)
      params <- T.load @(T.Tensors (Model N Device)) path >>= hmapM' T.MakeIndependent
      pure $ T.replaceParameters model params
    Nothing -> T.sample (ModelSpec @N)

  let optim0 = T.mkAdam 0 0.9 0.999 (T.flattenParameters model0)

  void . forkIO $ do
    putStrLn "Training..."
    (model, _, _) <- foldLoop (model0, optim0, 0) epoch $ \(model, optim, baseline) i -> do
      let gamma = 0.99 :: Float

      chAction <- newChan
      writeChan chReq chAction

      chObservation <- readChan chResp

      (episode, (totalScore, _)) <- fmap (first (($ mempty) . appEndo))
        . flip runTardisT (0 :: Float, 0 :: Int)
        . execWriterT
        . runMaybeT
        $ for_ [0 ..] \_ -> do
          Just obs <- liftIO $ readChan chObservation

          unless obs.isAlive $ sendPast 0 *> mzero

          [(action, logProb)] <- liftIO $ (V.toList <$> sampleAction @1 model [obs])
          liftIO $ writeChan chAction action

          modifyBackwards (* gamma)
          reward <- getFuture
          if obs.hasEaten
            then do
              modifyBackwards (+ 1)
              sendFuture 0
            else do
              noEatingCount <- getPast
              when (noEatingCount >= n * n) mzero
              modifyForwards (+ 1)

          lift $ tell $ Endo ((action, reward, logProb) :)
          pure reward

      let (rewards, logProbs) = unzip $ (\(_, reward, logProb) -> (max 0 (reward - baseline), logProb)) <$> episode
          logProbs' = UT.stack (UT.Dim 0) (T.toDynamic <$> logProbs)
          rewards' = UT.toDevice (UT.device logProbs') $ UT.asTensor rewards

      let loss = T.UnsafeMkTensor $ UT.sumAll $ rewards' * logProbs' :: T.Tensor Device T.Float '[]
      let baseline' = 0.99 * baseline + 0.01 * totalScore

      liftIO $ hPrint stderr (i, totalScore, baseline')
      (model', optim') <- T.runStep model optim (-loss) (realToFrac learningRate)

      pure (model', optim', baseline')

    mapM_ (T.save $ hmap' T.ToDependent $ T.flattenParameters model) savePath

  mainWidget $ initManager_ $ mdo
    (eChAction, triggerChAction) <- newTriggerEvent
    dChAction <- holdDyn Nothing (leftmost [eChAction, eDied $> Nothing])
    void . liftIO . forkIO . forever $ readChan chReq >>= triggerChAction . Just

    eDied <-
      tile (fixed $ pure $ n + 2) . row . tile (fixed $ pure $ n + 2) $
        boxStatic def $
          (switchHoldPromptly never . catMaybes <=< networkView) $
            dChAction `ffor` mapM \chAction -> do
              chObservation <- liftIO newChan
              liftIO $ writeChan chResp chObservation

              eAction <- newEventWithLazyTriggerWithOnComplete $ \triggerAction -> do
                thread <-
                  liftIO . forkIO . forever $
                    readChan chAction >>= flip triggerAction (pure ())
                pure (killThread thread)

              eObs <- delay' bDelay =<< snakeGame @N eAction
              let eDied = ffilter isNothing eObs $> ()
              performEvent_ $ eObs `ffor` (liftIO . writeChan chObservation)
              pure eDied

    eInput <- input

    -- TODO: delay보단 speed로
    bDelay <- accumB (\x y -> max 0 $ x + y) 0 $ leftmost [eSlowDown $> 0.01, eSpeedUp $> -0.01]
    let eSlowDown = fforMaybe eInput $ \case
          V.EvKey V.KDown [] -> Just ()
          _ -> Nothing
    let eSpeedUp = fforMaybe eInput $ \case
          V.EvKey V.KUp [] -> Just ()
          _ -> Nothing

    let eCtrlC = fforMaybe eInput $ \case
          V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
          _ -> Nothing

    pure $ eCtrlC $> ()

--  <> (b $> ())

data Model (n :: Nat) (device :: (T.DeviceType, Nat)) = Model
  { layer1 :: T.Linear (3 * (n * n)) (2 * (n * n)) T.Float device,
    layer2 :: T.Linear (2 * (n * n)) (n * n) T.Float device,
    layer3 :: T.Linear (n * n) 4 T.Float device
  }
  deriving (Generic)

data ModelSpec (n :: Nat) (device :: (T.DeviceType, Nat)) = ModelSpec

instance (T.KnownDevice device, KnownNat (n * n), KnownNat (2 * (n * n)), KnownNat (3 * (n * n))) => T.Randomizable (ModelSpec n device) (Model n device) where
  sample _ = do
    layer1 <- T.sample (T.KamimingUniform UT.FanIn UT.Relu :: T.LinearSpec' (3 * (n * n)) (2 * (n * n)) T.Float device)
    layer2 <- T.sample (T.KamimingUniform UT.FanIn UT.Relu :: T.LinearSpec' (2 * (n * n)) (n * n) T.Float device)
    layer3 <- T.sample (T.KamimingUniform UT.FanIn UT.Relu :: T.LinearSpec' (n * n) 4 T.Float device)
    pure Model {..}

instance T.Parameterized (Model n device)

sampleAction ::
  forall batchSize n device.
  ( KnownNat batchSize,
    batchSize ~ 1,
    KnownNat n,
    KnownNat (n * n),
    KnownNat (3 * (n * n)),
    T.KnownDevice device,
    T.StandardFloatingPointDTypeValidation device T.Float
  ) =>
  Model n device ->
  [Observation n] ->
  IO (Vector batchSize (Grid.Direction, T.Tensor device T.Float '[]))
sampleAction Model {..} obss = do
  let probss :: T.Tensor device T.Float '[batchSize, 4] =
        T.linearForward' layer1 (T.reshape @'[batchSize, 3 * (n * n)] inputs)
          & T.relu
          & T.linearForward' layer2
          & T.relu
          & T.linearForward' layer3
          & T.softmax @1
  for (fromJust $ V.fromList @batchSize $ finites @batchSize) $ \i -> do
    let probs = T.selectIdx @0 probss i
    (toEnum &&& (T.log . T.selectIdx @0 probs . fromIntegral)) . T.toInt <$> T.multinomial @1 probs
  where
    n = T.natValI @n
    inputs :: T.Tensor device T.Float '[batchSize, 3, n, n] = T.vecStack @0 $ fromJust $ V.fromList $ encode <$> obss
    encode :: Observation n -> T.Tensor device T.Float '[3, n, n]
    encode obs = T.stack @0 $ headEncoding T.:. foodEncoding T.:. bodyEncoding T.:. T.HNil
      where
        body = Set.fromList $ snakeBody obs.snake
        headEncoding :: T.Tensor device T.Float '[n, n] =
          T.reshape @_ @'[n, n]
            . T.toDType @T.Float @T.Bool
            . T.UnsafeMkTensor
            $ UT.oneHot
              (n * n)
              (T.toDynamic (fromJust $ fromList [r * n + c :: Int] :: T.Tensor device T.Int64 '[1]))
          where
            (r, c) = obs.snake.head
        foodEncoding :: T.Tensor device T.Float '[n, n] =
          T.reshape @_ @'[n, n]
            . T.toDType @T.Float @T.Bool
            . T.UnsafeMkTensor
            $ UT.oneHot
              (n * n)
              (T.toDynamic (fromJust $ fromList [r * n + c :: Int] :: T.Tensor device T.Int64 '[1]))
          where
            (r, c) = bimap fromIntegral fromIntegral obs.food
        bodyEncoding :: T.Tensor device T.Float '[n, n] =
          T.reshape @_ @'[n, n]
            . T.toDType @T.Float @T.Bool
            $ fromJust (fromList [[(r, c) `Set.member` body | c <- [0 .. n - 1]] | r <- [0 .. n - 1]])
