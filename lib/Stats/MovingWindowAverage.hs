module Stats.MovingWindowAverage where

import Data.Sequence as S
import Reflex

data Window a = Window {size :: Int, sum' :: a, data' :: Seq a}
  deriving (Show)

empty :: (Num a) => Int -> Window a
empty size = Window size 0 Empty

average :: (Num a, Fractional a) => Window a -> a
average Window {sum', data'} = sum' / fromIntegral (S.length data')

isFull :: Window a -> Bool
isFull (Window size _ data') = S.length data' == size

newtype WindowPatch a = Enqueue a

instance (Num a) => Patch (WindowPatch a) where
  type PatchTarget (WindowPatch a) = Window a

  apply (Enqueue x) (Window size sum' data')
    | S.length data' < size = Just $ Window size (sum' + x) (x :<| data')
    | (ys :|> y) <- data' = Just $ Window size (sum' + x - y) (x :<| ys)
    | otherwise = error "impossible"
