import Control.Arrow
import Control.Arrow.TorchArrow
import Torch hiding (add, mul)

op :: TorchArrow (Tensor, Tensor) Tensor
op = proc (x1, x2) -> do
  y1 <- add -< (x1, asTensor [2 :: Float])
  y2 <- mul -< (x2, asTensor [-3 :: Float])
  returnA <<< add -< (y1, y2)

main :: IO ()
main =
  print =<< runTorchArrow op (asTensor [1 :: Float], asTensor [2 :: Float])
