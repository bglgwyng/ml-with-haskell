module Reflex.Random where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Set
import Data.Set qualified as Set
import Reflex
import System.Random

sampleFromSet :: (RandomGen g) => Set a -> g -> Maybe (a, g)
sampleFromSet set gen
  | Set.null set = Nothing
  | otherwise = Just $ sampleFromList (Set.toList set) gen

sampleFromList :: (RandomGen g) => [a] -> g -> (a, g)
sampleFromList xs gen =
  let (index, newGen) = randomR (0, length xs - 1) gen
   in (xs !! index, newGen)

holdSampler :: (Reflex t, Monad m, MonadIO m, MonadSample t m, Random a, MonadFix m, MonadHold t m, RandomGen g) => Behavior t (g -> (a, g)) -> Event t () -> g -> m (Behavior t a)
holdSampler bSampler eNext gen = mdo
  sampler <- sample bSampler
  bValue <- hold (sampler gen) eValue
  let eValue = pushAlways (const (sample bSampler <*> (snd <$> sample bValue))) eNext
  pure (fst <$> bValue)
