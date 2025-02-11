{-# LANGUAGE UndecidableInstances #-}

module RL.Agents.WASD where

import Control.Arrow ((&&&))
import Data.Bifunctor
import Data.Finite
import Data.Function
import Data.Maybe
import GHC.Generics
import GHC.IsList
import GHC.TypeNats
import Grid qualified
import Torch qualified as UT
import Torch.Initializers qualified as UT
import Torch.Typed qualified as T
import Torch.Typed.Extra qualified as T
