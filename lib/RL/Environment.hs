module RL.Environment where

class Environment e where
  type Observation e
  type Action e

  isTerminal :: e -> Bool
  observe :: e -> Observation e
  step :: (Monad m) => e -> (Observation e -> m (Action e, a)) -> m (Action e, e, a)

runAgent :: (Environment e, Monad m) => e -> (Observation e -> m (Action e, a)) -> m [(Action e, Observation e, a)]
runAgent env _
  | isTerminal env = pure []
runAgent env f = do
  (action, env', a) <- step env f
  ((action, observe env', a) :) <$> runAgent env' f

data WithMaxStep e = WithMaxStep {env :: e, maxStep :: Int}

instance (Environment e) => Environment (WithMaxStep e) where
  type Observation (WithMaxStep e) = (Int, Observation e)
  type Action (WithMaxStep e) = Action e

  isTerminal e = e.maxStep <= 0 || isTerminal e.env
  observe e = (e.maxStep, observe e.env)
  step e f = do
    (action, env', a) <- step e.env (f . (e.maxStep,))
    pure (action, WithMaxStep {env = env', maxStep = e.maxStep - 1}, a)
