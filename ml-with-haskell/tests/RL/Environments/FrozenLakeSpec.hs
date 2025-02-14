module RL.Environments.FrozenLakeSpec where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Finite
import Data.Function
import Data.Maybe
import Data.Vector.Sized qualified as V
import RL.Environments.FrozenLake (Tile (..), isReachable)
import RL.Environments.FrozenLake qualified as FL
import System.Random
import System.Random.Stateful (Uniform (uniformM), runStateGen, runStateGen_)
import Test.Hspec
import Torch.Typed (Randomizable (..))

spec :: Spec
spec =
  describe "FrozenLake Environment" $ do
    describe "isReachable" $ do
      it "should return True for reachable maps" $ do
        let map =
              fromJust . V.fromList $
                fromJust . V.fromList
                  <$> [ [F, F],
                        [F, F]
                      ]
            state = FL.FrozenLake {map, position = (0, 0)} :: FL.FrozenLake 2
        isReachable state `shouldBe` True

      it "should return False for unreachable maps" $ do
        let map =
              fromJust . V.fromList $
                fromJust . V.fromList
                  <$> [ [F, H],
                        [H, F]
                      ]
            state = FL.FrozenLake {map, position = (0, 0)} :: FL.FrozenLake 2
        isReachable state `shouldBe` False

      it "should return True when there is exactly one path" $ do
        let map =
              fromJust . V.fromList $
                fromJust . V.fromList
                  <$> [ [F, F, F],
                        [H, H, F],
                        [H, H, F]
                      ]
            state = FL.FrozenLake {map, position = (0, 0)} :: FL.FrozenLake 3
        isReachable state `shouldBe` True

      it "should return False when goal is surrounded by holes" $ do
        let map =
              fromJust . V.fromList $
                fromJust . V.fromList
                  <$> [ [F, F, F],
                        [F, F, H],
                        [F, H, F]
                      ]
            state = FL.FrozenLake {map, position = (0, 0)} :: FL.FrozenLake 3
        isReachable state `shouldBe` False
      it "should return True for a typical 4x4 map" $ do
        let map =
              fromJust . V.fromList $
                fromJust . V.fromList
                  <$> [ [F, F, F, F],
                        [F, H, F, H],
                        [F, F, F, H],
                        [H, F, F, F]
                      ]
            state = FL.FrozenLake {map, position = (0, 0)} :: FL.FrozenLake 4
        isReachable state `shouldBe` True
    describe "Randomizable Instance" $ do
      beforeAll @[FL.FrozenLake 4]
        ( flip runStateGen_ (replicateM 10 . uniformM) <$> newStdGen
        )
        $ do
          it "should generate states with F tile at start position (0, )" $ \states -> do
            forAll states $ \state -> do
              (state.map `V.index` 0 `V.index` 0) `shouldBe` F
          it "should generate states with F tile at goal position (n - 1, n - 1)" $ \states -> do
            forAll states $ \state -> do
              let (r, c) = FL.goalPosition
              (state.map `V.index` r `V.index` c) `shouldBe` F
          it "should generate only states with reachable goals" $ \states -> do
            forAll states $ \state -> do
              isReachable state `shouldBe` True

forAll :: [a] -> (a -> IO ()) -> IO ()
forAll xs f = mapM_ f xs
