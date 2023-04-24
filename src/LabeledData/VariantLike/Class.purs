module LabeledData.VariantLike.Class where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Variant (Variant)
import Data.Variant as V
import LabeledData.VariantLike.Generic (class GenericVariantLike, genericFromVariant, genericToVariant)
import LabeledData.VariantLike.Transform (LowerFirst(..))
import Type.Proxy (Proxy(..))

class VariantLike a r | a -> r where
  toVariant :: a -> Variant r
  fromVariant :: Variant r -> a

instance VariantLike (Variant r) r where
  toVariant = identity
  fromVariant = identity

instance (GenericVariantLike LowerFirst (Tuple a b) r) => VariantLike (Tuple a b) r where
  toVariant = genericToVariant LowerFirst
  fromVariant = genericFromVariant LowerFirst

instance VariantLike Boolean ("true" :: Unit, false :: Unit) where
  toVariant =
    if _ then
      V.inj (Proxy :: _ "true") unit
    else
      V.inj (Proxy :: _ "false") unit

  fromVariant = V.case_
    # V.on (Proxy :: _ "true") (const true)
    # V.on (Proxy :: _ "false") (const false)

instance (GenericVariantLike LowerFirst (Either a b) r) => VariantLike (Either a b) r where
  toVariant = genericToVariant LowerFirst
  fromVariant = genericFromVariant LowerFirst

instance (GenericVariantLike LowerFirst (Maybe a) r) => VariantLike (Maybe a) r where
  toVariant = genericToVariant LowerFirst
  fromVariant = genericFromVariant LowerFirst
