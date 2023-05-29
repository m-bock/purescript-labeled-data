module LabeledData.TransformEntry where

import Data.Tuple.Nested (type (/\))
import Type.Proxy (Proxy(..))

class TransformEntry :: Type -> Symbol -> Symbol -> Type -> Type -> Constraint
class TransformEntry tok sym sym' a a' | tok sym a -> sym' a' where
  transformEntry
    :: Proxy tok -> Proxy sym -> Proxy sym' -> a -> a'
  untransformEntry
    :: Proxy tok -> Proxy sym' -> Proxy sym -> a' -> a

instance
  ( TransformEntry tokB sym sym' a a'
  , TransformEntry tokA sym' sym'' a' a''
  ) =>
  TransformEntry (tokA /\ tokB) sym sym'' a a'' where
  transformEntry _ _ _ x =
    let
      x' :: a'
      x' = transformEntry (Proxy :: _ tokB) (Proxy :: _ sym) (Proxy :: _ sym') x
    in
      transformEntry (Proxy :: _ tokA) (Proxy :: _ sym') (Proxy :: _ sym'') x'

  untransformEntry _ _ _ x =
    let
      x' :: a'
      x' = untransformEntry (Proxy :: _ tokA) (Proxy :: _ sym'') (Proxy :: _ sym') x
    in
      untransformEntry (Proxy :: _ tokB) (Proxy :: _ sym') (Proxy :: _ sym) x'
