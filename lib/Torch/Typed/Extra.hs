module Torch.Typed.Extra where

import Data.Finite (Finite)
import GHC.TypeLits
import Torch qualified as UT
import Torch.Initializers qualified as UT
import Torch.Typed

mulScalar' ::
  forall
    a
    (shape :: [Nat])
    (dtype :: DType)
    (device :: (DeviceType, Nat)).
  (Scalar a) =>
  Tensor device dtype shape ->
  a ->
  Tensor device dtype shape
mulScalar' = flip mulScalar

addScalar' ::
  forall
    a
    (shape :: [Nat])
    (dtype :: DType)
    (device :: (DeviceType, Nat)).
  (Scalar a) =>
  Tensor device dtype shape ->
  a ->
  Tensor device dtype shape
addScalar' = flip addScalar

subScalar' ::
  forall
    a
    (shape :: [Nat])
    (dtype :: DType)
    (device :: (DeviceType, Nat)).
  (Scalar a) =>
  Tensor device dtype shape ->
  a ->
  Tensor device dtype shape
subScalar' = flip subScalar

divScalar' ::
  forall
    a
    (shape :: [Nat])
    (dtype :: DType)
    (device :: (DeviceType, Nat)).
  (Scalar a) =>
  Tensor device dtype shape ->
  a ->
  Tensor device dtype shape
divScalar' = flip divScalar

expand' ::
  forall
    (shape' :: [Nat])
    (shape :: [Nat])
    (dtype :: DType)
    (device :: (DeviceType, Nat)).
  (KnownShape shape', shape' ~ Broadcast shape shape') =>
  Tensor device dtype shape ->
  Tensor device dtype shape'
expand' = expand False

data LinearSpec' inputFeatures outputFeatures dtype device = KamimingUniform {fanMode :: UT.FanMode, nonLinearity :: UT.NonLinearity}

multinomial ::
  forall n (length :: Nat) (dtype :: DType) (device :: (DeviceType, Nat)).
  (KnownNat n) =>
  Tensor device dtype '[length] ->
  IO (Tensor device dtype '[n])
multinomial t = UnsafeMkTensor <$> UT.multinomialIO' (toDynamic t) (natValI @n)
