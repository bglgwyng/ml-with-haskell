module Grid where

import Data.Patch
import System.Random (Uniform)

type Position = (Int, Int)

data Direction = Up | Down | Left | Right deriving (Show, Read, Eq, Ord, Bounded, Enum)

instance Patch Direction where
  type PatchTarget Direction = Position
  apply dir (x, y) = pure (x + dx, y + dy)
    where
      (dx, dy) = delta dir

opposite :: Direction -> Direction
opposite = \case
  Up -> Down
  Down -> Up
  Grid.Left -> Grid.Right
  Grid.Right -> Grid.Left

delta :: Direction -> (Int, Int)
delta = \case
  Up -> (-1, 0)
  Down -> (1, 0)
  Grid.Left -> (0, -1)
  Grid.Right -> (0, 1)