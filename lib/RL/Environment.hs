module RL.Environment where

import Control.Monad.RWS (MonadTrans)
import Data.Kind

class (Monad (Effect e)) => Environment e where
  type Observation e
  type Action e
  type Effect e :: Type -> Type

  isTerminal :: e -> Bool
  observe :: e -> Observation e
  step :: (Monad m) => e -> (forall x. Effect e x -> m x) -> (Observation e -> m (Action e, a)) -> m (Action e, e, a)

runAgent :: (Environment e, Monad m) => e -> (forall x. Effect e x -> m x) -> (Observation e -> m (Action e, a)) -> m [(Action e, Observation e, a)]
runAgent env _ _
  | isTerminal env = pure []
runAgent env lift f = do
  (action, env', a) <- step env lift f
  ((action, observe env', a) :) <$> runAgent env' lift f

data WithMaxStep e = WithMaxStep {env :: e, maxStep :: Int}

instance (Environment e) => Environment (WithMaxStep e) where
  type Observation (WithMaxStep e) = (Int, Observation e)
  type Action (WithMaxStep e) = Action e
  type Effect (WithMaxStep e) = Effect e

  isTerminal e = e.maxStep <= 0 || isTerminal e.env
  observe e = (e.maxStep, observe e.env)
  step e lift f = do
    (action, env', a) <- step e.env lift (f . (e.maxStep,))
    pure (action, WithMaxStep {env = env', maxStep = e.maxStep - 1}, a)
