module LabeledData.VariantLike.Transform where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\))
import Prim.Row as Row
import Prim.Symbol as Sym
import Record as Record
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))

class TransformVariantCase :: Type -> Symbol -> Symbol -> Type -> Type -> Constraint
class TransformVariantCase tok sym sym' a a' | tok sym a -> sym' a' where
  transformToVariant
    :: Proxy tok -> Proxy sym -> Proxy sym' -> a -> a'
  transformFromVariant
    :: Proxy tok -> Proxy sym' -> Proxy sym -> a' -> a

---
data LowerFirst = LowerFirst

instance LowerFirstCl sym sym' => TransformVariantCase LowerFirst sym sym' a a where
  transformToVariant _ _ _ = identity
  transformFromVariant _ _ _ = identity

---
data SingleField = SingleField

instance
  ( Row.Cons symFirst a () r
  , TypeEquals symFirst "_1"
  , IsSymbol symFirst
  ) =>
  TransformVariantCase SingleField sym sym (Record r) a where
  transformToVariant _ _ _ = Record.get (Proxy :: _ symFirst)
  transformFromVariant _ _ _ x = Record.insert (Proxy :: _ symFirst) x {}

---

data NoTransform = NoTransform

instance TransformVariantCase NoTransform sym sym a a where
  transformToVariant _ _ _ = identity
  transformFromVariant _ _ _ = identity

---

instance
  ( TransformVariantCase tokB sym sym' a a'
  , TransformVariantCase tokA sym' sym'' a' a''
  ) =>
  TransformVariantCase (tokA /\ tokB) sym sym'' a a'' where
  transformToVariant _ _ _ x =
    let
      x' :: a'
      x' = transformToVariant (Proxy :: _ tokB) (Proxy :: _ sym) (Proxy :: _ sym') x
    in
      transformToVariant (Proxy :: _ tokA) (Proxy :: _ sym') (Proxy :: _ sym'') x'

  transformFromVariant _ _ _ x =
    let
      x' :: a'
      x' = transformFromVariant (Proxy :: _ tokA) (Proxy :: _ sym'') (Proxy :: _ sym') x
    in
      transformFromVariant (Proxy :: _ tokB) (Proxy :: _ sym') (Proxy :: _ sym) x'

--- LowerFirst

class LowerFirstCl (sym1 :: Symbol) (sym2 :: Symbol) | sym2 -> sym1

instance
  ( Sym.Cons head tail sym1
  , ToLower head head'
  , Sym.Cons head' tail sym2
  ) =>
  LowerFirstCl sym1 sym2

--- ToLower

class ToLower (sym1 :: Symbol) (sym2 :: Symbol) | sym1 -> sym2

instance ToLower "A" "a"
instance ToLower "B" "b"
instance ToLower "C" "c"
instance ToLower "D" "d"
instance ToLower "E" "e"
instance ToLower "F" "f"
instance ToLower "G" "g"
instance ToLower "H" "h"
instance ToLower "I" "i"
instance ToLower "J" "j"
instance ToLower "K" "k"
instance ToLower "L" "l"
instance ToLower "M" "m"
instance ToLower "N" "n"
instance ToLower "O" "o"
instance ToLower "P" "p"
instance ToLower "Q" "q"
instance ToLower "R" "r"
instance ToLower "S" "s"
instance ToLower "T" "t"
instance ToLower "U" "u"
instance ToLower "V" "v"
instance ToLower "W" "w"
instance ToLower "X" "x"
instance ToLower "Y" "y"
instance ToLower "Z" "z"
