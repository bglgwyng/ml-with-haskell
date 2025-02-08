import Control.Monad.IO.Class
import Control.Monad.Tardis
import Data.Foldable (for_)
import Data.Traversable (for)
import GHC.IO (unsafePerformIO)

type Obs = Int

type Action = Int

observe :: (MonadTardis a Obs m) => m Obs
observe = getPast

chooseAction :: (MonadTardis a b m) => Obs -> m (Action, Float)
chooseAction = undefined

main :: IO ()
main = do
  x <-
    flip runTardisT (0, 0) $ mdo
      for [(1 :: Int) .. 10] $ \i -> do
        result <- getFuture
        -- let result = 0
        -- obs <- observe
        -- (action, prob) <- chooseAction obs

        -- sendFuture (2)
        -- getPast >>= sendFuture . (+ 2)

        -- sendPast . (+ 3) =<< getFuture
        -- sendPast 10
        y <- getFuture
        -- pure $ seq (unsafePerformIO $ print "!11") $ Just ()
        -- sendPast (y + 20)
        modifyBackwards (+ 1)
        -- liftIO $ print 1
        -- getPast
        -- sendPast 1
        pure result
  -- pure 0

  print x
  putStrLn "Hello, World!"
