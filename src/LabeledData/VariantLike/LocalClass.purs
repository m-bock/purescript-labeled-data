module LabeledData.VariantLike.LocalClass where

import Data.Variant (Variant)

class VariantLike :: Type -> Type -> Row Type -> Constraint
class VariantLike tok a r | tok a -> r where
  toVariant :: tok -> a -> Variant r
  fromVariant :: tok -> Variant r -> a
