module RL.Environments.FrozenLake where

import Control.Monad
import Control.Monad.IO.Class
import Data.Graph
import System.Random
import System.Random.Stateful
import Torch.Typed (Randomizable (..))

-- | Game state representation
data Tile = F | H deriving (Show, Enum, Bounded, Eq)

data State = State
  { map :: [[Tile]],
    position :: (Int, Int)
  }

instance Randomizable Int State where
  sample n = do
    stdGen <- newStdGen
    map <-
      runStateGenT_
        stdGen
        ( \g -> do
            let tryGenerate = do
                  row0 :: [Tile] <- fmap (F :) $ replicateM (n - 1) $ uniformEnumM g
                  row1 :: [[Tile]] <-
                    replicateM (n - 2)
                      . replicateM n
                      $ uniformEnumM g
                  row2 :: [Tile] <- fmap (<> [F]) $ replicateM (n - 1) $ uniformEnumM g
                  let map = [row0] <> row1 <> [row2]
                  let state = State {map = map, position = (0, 0)}
                  liftIO $ print (isReachable state, state.map)

                  if isReachable state
                    then pure map
                    else tryGenerate
            tryGenerate
        )
    pure State {map = map, position = (0, 0)}

-- | Available actions in the game
data Action = GoLeft | GoRight | GoUp | GoDown
  deriving (Eq, Enum)

instance Show Action where
  show GoLeft = "L"
  show GoRight = "R"
  show GoUp = "U"
  show GoDown = "D"

-- | Move the agent according to the given action
move :: State -> Action -> State
move State {position = (r, c), ..} action =
  State
    { position = case action of
        GoLeft -> (r, max 0 (c - 1))
        GoRight -> (r, min (width - 1) (c + 1))
        GoUp -> (max 0 (r - 1), c)
        GoDown -> (min (height - 1) (r + 1), c),
      ..
    }
  where
    width = length (head map)
    height = length map

-- | Check if the current state is terminal and get reward
isTerminal :: State -> Bool
isTerminal State {map, position = (r, c)} =
  n - 1 == r && n - 1 == c
  where
    n = length map

isReachable :: State -> Bool
isReachable State {map} =
  0 `elem` reachable graph ((n - 1) * n + (n - 1))
  where
    n = length map
    edges =
      [ (i * n + j, i' * n + j')
        | i <- [0 .. n - 1],
          j <- [0 .. n - 1],
          (i', j') <- [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)],
          i' >= 0 && i' < n,
          j' >= 0 && j' < n,
          map !! i !! j /= H,
          map !! i' !! j' /= H
      ]
    graph = buildG (0, n * n - 1) edges
