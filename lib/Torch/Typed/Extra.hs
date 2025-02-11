module Torch.Typed.Extra where

import Data.Kind
import GHC.TypeLits
import Torch (ATenTensor)
import Torch qualified as UT
import Torch.Initializers qualified as UT
import Torch.Internal.Class
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

multinomial ::
  forall n (length :: Nat) (dtype :: DType) (device :: (DeviceType, Nat)).
  (KnownNat n) =>
  Tensor device dtype '[length] ->
  IO (Tensor device dtype '[n])
multinomial t = UnsafeMkTensor <$> UT.multinomialIO' (toDynamic t) (natValI @n)

data LinearSpec' inputFeatures outputFeatures dtype device = KamimingUniform {fanMode :: UT.FanMode, nonLinearity :: UT.NonLinearity}

instance
  (KnownNat inputFeatures, KnownNat outputFeatures, KnownDType dtype, KnownDevice device) =>
  Randomizable (LinearSpec' inputFeatures outputFeatures dtype device) (Linear inputFeatures outputFeatures dtype device)
  where
  sample (KamimingUniform {..}) = do
    weight <- makeIndependent . UnsafeMkTensor . UT.toDevice (deviceVal @device) =<< UT.kaimingUniform fanMode nonLinearity [natValI @outputFeatures, natValI @inputFeatures]
    bias <- makeIndependent (zeros @'[outputFeatures] @dtype @device)
    pure $ Linear {..}

-- type C a b  = Castable b a
-- loadParams ::
--   forall b.
--   ( Parameterized b,
--     Castable (HList (Tensors b)) [ATenTensor],
--     HMapM' IO MakeIndependent (Tensors' (Parameters b)) (Parameters b)
--   ) =>
--   -- | model
--   b ->
--   -- | filepath
--   FilePath ->
--   -- | output
--   IO b
-- loadParams model filePath = do
--   tensors <- load @(Tensors b) filePath
--   params :: HList (Parameters b) <- hmapM' MakeIndependent tensors
--   pure $ replaceParameters model params

-- TODO: https://github.com/hasktorch/hasktorch/commit/de0d9284becc16c9588107616843cc10bf15b297 적용하고 없애기
type family Tensors' (p :: [Type]) :: [Type] where
  Tensors' '[] = '[]
  Tensors' (Parameter device dtype shape ': ps) = Tensor device dtype shape ': Tensors' ps

type Tensors (p :: Type) = Tensors' (Parameters p)

-- instance Parameterized p => Parameterized (HList (Tensors p)) where
--   type Parameters (HList (Tensors p)) = HList (Parameters p)
--   flattenParameters = hmap parameters

linearForward' ::
  forall (batchSize :: Nat) (inputFeatures :: Nat) (outputFeatures :: Nat) (dtype :: DType) (device :: (DeviceType, Nat)).
  Linear inputFeatures outputFeatures dtype device ->
  Tensor device dtype '[batchSize, inputFeatures] ->
  Tensor device dtype '[batchSize, outputFeatures]
linearForward' lin = linear @batchSize (toDependent lin.weight) (toDependent lin.bias)
