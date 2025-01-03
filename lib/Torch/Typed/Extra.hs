module Torch.Typed.Extra where

import GHC.TypeLits
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
