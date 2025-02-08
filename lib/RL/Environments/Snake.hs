{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module RL.Environments.Snake where

import Control.Arrow (second)
import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.Finite
import Data.Function
import Data.Functor
import Data.Maybe
import Data.Patch
import Data.Proxy
import Data.Set qualified as Set
import Data.These
import Debug.Trace (traceShowM)
import GHC.TypeNats
import Graphics.Vty hiding (Event)
import Grid
import RL.Agent
import Reflex
import Reflex.Random
import Reflex.Vty
import Reflex.Vty qualified as Grid
import Reflex.Vty.Utils
import System.IO
import System.Random hiding (Finite)

data Snake = Snake
  { head :: Position,
    direction :: Direction,
    spine :: [(Direction, Int)]
  }
  deriving (Show)

snakeBody :: Snake -> [Position]
snakeBody (Snake pos _ spine) = concat $ scanl moveSegment [pos] spine
  where
    moveSegment points (direction, steps) =
      take steps $ tail $ iterate (applyAlways (opposite direction)) (last points)

data SnakeMove
  = Advance
  | Turn Direction

advanceHead :: Snake -> Snake
advanceHead (Snake pos direction spine) =
  Snake newPos direction updatedSpine
  where
    newPos = applyAlways direction pos
    updatedSpine = case spine of
      (dir, steps) : rest
        | dir == direction -> (dir, steps + 1) : rest
      _ -> (direction, 1) : spine

shrinkTail :: Snake -> Maybe Snake
shrinkTail snake@(Snake {spine}) =
  case spine of
    [] -> Nothing
    [(_, 1)] -> Nothing
    [(dir, steps)] -> Just $ snake {spine = [(dir, steps - 1)]}
    rest -> Just $ snake {spine = init rest <> lastSegment}
      where
        lastSegment = case last rest of
          (_, 1) -> []
          (dir, steps) -> [(dir, steps - 1)]

data Observation (n :: Nat) = Observation
  { snake :: Snake,
    food :: (Finite n, Finite n),
    isAlive :: Bool,
    hasEaten :: Bool
  }
  deriving (Show)

type Action = Grid.Direction

instance (KnownNat n, MonadReader (Chan (Observation n), Chan Action) m, MonadIO m) => MonadAgent (Observation n) Action m where
  observe = do
    (channel, _) <- ask
    liftIO $ readChan channel
  act action = do
    (_, channel) <- ask
    liftIO $ writeChan channel action

snakeGame :: forall n t m. (KnownNat n, VtyExample t m) => Event t Grid.Direction -> m (Event t (Maybe (Observation n)))
snakeGame eAction = do
  foodGen <- liftIO newStdGen
  mdo
    let eTick = eAction

    let eTurn =
          push
            ( \action -> runMaybeT do
                snake <- lift $ sample $ current dSnake
                guard (action /= opposite (direction snake))
                pure action
            )
            eAction

    -- performEvent_ $ eTick `ffor` (liftIO . hPrint stderr . ("###",) . (show))
    -- performEvent_ $ eProceed `ffor` (liftIO . hPrint stderr . ("###",) . (show))

    dSnake <- holdDyn initialSnake eMoveSnake
    bFood <-
      holdSampler
        ( do
            body <- Set.fromList . snakeBody <$> current dSnake
            let freeSpaces = [(y, x) | y <- [0 .. height - 1], x <- [0 .. width - 1], (y, x) `Set.notMember` body]
            pure (sampleFromList freeSpaces)
        )
        eFood
        foodGen

    let (eMoveSnake, eFood) = second (void . ffilter id) . splitE $ eProceed
    let eProceed =
          pushAlways
            ( \direction -> do
                snake <- sample $ current dSnake
                food <- sample bFood

                let newSnake = advanceHead $ snake {direction}
                let isFoodEaten = newSnake.head == food

                pure (applyWhen (not isFoodEaten) (fromJust . shrinkTail) newSnake, isFoodEaten)
            )
            ( leftmost
                [ eTurn,
                  tag (direction <$> current dSnake) eTick
                ]
            )

    let eDied =
          void
            $ ffilter
              ( \snake ->
                  let (y, x) = snake.head
                   in Set.member snake.head (Set.fromList $ tail $ snakeBody snake) || (y < 0 || y >= height || x < 0 || x >= width)
              )
            $ updated dSnake

    col . grout (fixed (constDyn height)) . row . grout (fixed (constDyn width)) $ do
      fillBoard
        ( \(x, y) -> do
            snake <- current dSnake
            food <- bFood
            if
              | (x, y) `elem` snakeBody snake -> pure (withForeColor defAttr yellow, 'O')
              | food == (x, y) -> pure (withForeColor defAttr brightGreen, '@')
              | otherwise -> pure (defAttr, ' ')
        )

    ePostBuild <- getPostBuild
    let eSnake = leftmost [updated dSnake, tag (current dSnake) ePostBuild]

    pure $
      leftmost
        [ eDied $> Nothing,
          flip pushAlways (alignEventWithMaybe thisMaybe eSnake eFood) $ \(snake, food) -> do
            (r, c) <- sample bFood
            pure $
              Just $
                Observation
                  { snake,
                    food = (fromIntegral r, fromIntegral c),
                    isAlive = True,
                    hasEaten = isJust food
                  }
        ]
  where
    height :: Int
    width :: Int
    (height, width) = (fromIntegral $ natVal (Proxy :: Proxy n), fromIntegral $ natVal (Proxy :: Proxy n))

    initialSnake :: Snake
    initialSnake = Snake (height `div` 2, width `div` 2) Grid.Right [(Grid.Right, 2)]

thisMaybe :: These a b -> Maybe (a, Maybe b)
thisMaybe (This a) = Just (a, Nothing)
thisMaybe (These a b) = Just (a, Just b)
thisMaybe (That _) = Nothing

type VtyExample t m =
  ( MonadFix m,
    MonadHold t m,
    Reflex t,
    HasInput t m,
    HasImageWriter t m,
    HasDisplayRegion t m,
    HasFocus t m,
    HasFocusReader t m,
    HasTheme t m,
    MonadNodeId m,
    HasLayout t m,
    MonadFix m,
    MonadHold t m,
    PerformEvent t m,
    TriggerEvent t m,
    MonadIO (Performable m),
    PostBuild t m,
    MonadIO m,
    Adjustable t m,
    NotReady t m
  )