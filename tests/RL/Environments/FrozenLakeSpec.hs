module RL.Environments.FrozenLakeSpec where

import Control.Monad
import Data.Function
import RL.Environments.FrozenLake (Tile (..), isReachable)
import RL.Environments.FrozenLake qualified as FL
import Test.Hspec
import Torch.Typed (Randomizable (..))

spec :: Spec
spec =
  describe "FrozenLake Environment" $ do
    describe "isReachable" $ do
      it "should return True for reachable maps" $ do
        let map =
              [ [F, F],
                [F, F]
              ]
            state = FL.State {map, position = (0, 0)}
        isReachable state `shouldBe` True

      it "should return False for unreachable maps" $ do
        let map =
              [ [F, H],
                [H, F]
              ]
            state = FL.State {map, position = (0, 0)}
        isReachable state `shouldBe` False

      it "should return True when there is exactly one path" $ do
        let map =
              [ [F, F, F],
                [H, H, F],
                [H, H, F]
              ]
            state = FL.State {map, position = (0, 0)}
        isReachable state `shouldBe` True

      it "should return False when goal is surrounded by holes" $ do
        let map =
              [ [F, F, F],
                [F, F, H],
                [F, H, F]
              ]
            state = FL.State {map, position = (0, 0)}
        isReachable state `shouldBe` False
      it "should return True for a typical 4x4 map" $ do
        let map =
              [ [F, F, F, F],
                [F, H, F, H],
                [F, F, F, H],
                [H, F, F, F]
              ]
            state = FL.State {map, position = (0, 0)}
        isReachable state `shouldBe` True
    describe "Randomizable Instance" $ do
      let n = 4 -- 일반적인 FrozenLake 크기
      beforeAll @[FL.State] (replicateM 10 (sample n)) $ do
        it "should generate states with correct n x n dimensions" $ \states -> do
          forAll states $ \state -> do
            length state.map `shouldBe` n
            all ((== n) . length) state.map `shouldBe` True
        it "should generate states with F tile at start position (0, )" $ \states -> do
          forAll states $ \state -> do
            (state.map & head & head) `shouldBe` F
        it "should generate states with F tile at goal position (n - 1, n - 1)" $ \states -> do
          forAll states $ \state -> do
            (state.map !! (n - 1) !! (n - 1)) `shouldBe` F
        it "should generate only states with reachable goals" $ \states -> do
          forAll states $ \state -> do
            isReachable state `shouldBe` True

forAll :: [a] -> (a -> IO ()) -> IO ()
forAll xs f = mapM_ f xs
