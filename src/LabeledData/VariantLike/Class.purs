module LabeledData.VariantLike.Class where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant)
import Data.Variant as V
import LabeledData.TransformEntry.Transforms (ArgsToRecord, LowerFirst, NoTransform, SingleField)
import LabeledData.VariantLike.Generic (class GenericVariantLike, genericFromVariant, genericToVariant)
import Type.Proxy (Proxy(..))

type DefaultTransform =
  LowerFirst
    /\ SingleField
    /\ (ArgsToRecord NoTransform)

class VariantLike a r | a -> r where
  toVariant :: a -> Variant r
  fromVariant :: Variant r -> a

--------------------------------------------------------------------------------
--- Instances
--------------------------------------------------------------------------------

instance VariantLike (Variant r) r where
  toVariant = identity
  fromVariant = identity

instance (GenericVariantLike DefaultTransform (Tuple a b) r) => VariantLike (Tuple a b) r where
  toVariant = genericToVariant (Proxy :: _ DefaultTransform)
  fromVariant = genericFromVariant (Proxy :: _ DefaultTransform)

---

type BooleanRow = (true :: Unit, false :: Unit)

type BooleanV = Variant BooleanRow

instance VariantLike Boolean BooleanRow where
  toVariant =
    if _ then
      V.inj (Proxy :: _ "true") unit
    else
      V.inj (Proxy :: _ "false") unit

  fromVariant = V.case_
    # V.on (Proxy :: _ "true") (const true)
    # V.on (Proxy :: _ "false") (const false)

---

type EitherRow :: Type -> Type -> Row Type
type EitherRow a b = (left :: a, right :: b)

type EitherV a b = Variant (EitherRow a b)

instance
  ( GenericVariantLike DefaultTransform (Either a b) (EitherRow a b)
  ) =>
  VariantLike (Either a b) (EitherRow a b)
  where
  toVariant = genericToVariant (Proxy :: _ DefaultTransform)
  fromVariant = genericFromVariant (Proxy :: _ DefaultTransform)

---

type MaybeRow :: Type -> Row Type
type MaybeRow a = (nothing :: Unit, just :: a)

type MaybeV a = Variant (MaybeRow a)

instance
  ( GenericVariantLike DefaultTransform (Maybe a) (MaybeRow a)
  ) =>
  VariantLike (Maybe a) (MaybeRow a)
  where
  toVariant = genericToVariant (Proxy :: _ DefaultTransform)
  fromVariant = genericFromVariant (Proxy :: _ DefaultTransform)

