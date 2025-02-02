{-# LANGUAGE UndecidableInstances #-}

module RL.Environments.FrozenLake (FrozenLake (..), Tile (..), Action (..), move, isReachable, goalPosition) where

import Control.Monad (join)
import Control.Monad.Identity (Identity)
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Finite
import Data.Functor.Identity
import Data.Graph
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as V
import Debug.Trace
import GHC.Generics (Generic)
import GHC.TypeLits
import RL.Agent
import System.Random hiding (Finite)
import System.Random.Stateful hiding (Finite)
import Torch.Typed qualified as T

-- | Game state representation
data Tile = F | H deriving (Show, Enum, Bounded, Eq)

data FrozenLake (n :: Nat) = FrozenLake
  { map :: Vector n (Vector n Tile),
    position :: (Finite n, Finite n)
  }
  deriving (Generic)

instance (KnownNat n) => Show (FrozenLake n) where
  show FrozenLake {map, position = (r, c)} =
    unlines $
      V.toList $
        V.imap
          ( \i row ->
              join $
                V.toList $
                  V.imap
                    ( \j tile ->
                        if (i, j) == (r, c)
                          then "A"
                          else show tile
                    )
                    row
          )
          map

instance (KnownNat n) => Uniform (FrozenLake n) where
  uniformM :: (StatefulGen g m) => g -> m (FrozenLake n)
  uniformM g = do
    let tryGenerate = do
          -- Use replicateM to generate the full 2D grid at once
          -- map <- V.replicateM $ V.replicateM $ uniformEnumM g
          map <- V.replicateM $ V.replicateM $ do
            tile <- uniformRM (0 :: Int, T.natValI @n - 1) g
            pure $ if tile == 0 then H else F
          let state = FrozenLake {map, position = (0, 0)}
          if isReachable state
            then pure map
            else tryGenerate
    map <- tryGenerate
    pure FrozenLake {map, position = (0, 0)}

-- pure undefined

-- | Available actions in the game
data Action = GoLeft | GoRight | GoUp | GoDown
  deriving (Eq, Enum, Bounded)

instance Show Action where
  show GoLeft = "L"
  show GoRight = "R"
  show GoUp = "U"
  show GoDown = "D"

-- | Move the agent according to the given action
move :: (KnownNat n) => (Finite n, Finite n) -> Action -> (Finite n, Finite n)
move (r, c) action =
  case action of
    GoLeft | c > minBound -> (r, c - 1)
    GoRight | c < maxBound -> (r, c + 1)
    GoUp | r > minBound -> (r - 1, c)
    GoDown | r < maxBound -> (r + 1, c)
    _ -> (r, c)

isReachable :: forall n. (KnownNat n) => FrozenLake n -> Bool
isReachable FrozenLake {map} =
  0 `elem` reachable graph (index goalPosition)
  where
    index :: (Finite n, Finite n) -> Int
    index (i, j) = fromIntegral i * n' + fromIntegral j
    n' = T.natValI @n
    edges =
      V.ifoldr
        ( \i row acc ->
            V.ifoldr
              ( \j tile acc' ->
                  if tile /= H
                    then
                      [ (index (i, j), index (i', j'))
                        | (i', j') <- move (i, j) <$> [minBound .. maxBound],
                          (i', j') /= (i, j),
                          map `V.index` i' `V.index` j' /= H
                      ]
                        ++ acc'
                    else acc'
              )
              acc
              row
        )
        []
        map
    graph = buildG (0, n' ^ 2 - 1) (edges)

goalPosition :: (KnownNat n) => (Finite n, Finite n)
goalPosition = (maxBound, maxBound)

instance (KnownNat n, MonadState (FrozenLake n) m) => MonadAgent (Finite n, Finite n) Action m where
  observe = gets position
  act action = modify $ \state -> state {position = move state.position action}