module LabeledData.TransformEntry.Transforms where


import Prelude

import Data.Symbol (class IsSymbol)
import LabeledData.RecordLike.Generic (class RepRecordLike, repFromRecord, repToRecord)
import LabeledData.TransformEntry (class TransformEntry)
import Prim.Row as Row
import Prim.Symbol as Sym
import Record as Record
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))

data Prefix (symPrefix :: Symbol)

instance PrefixCl symPrefix sym sym' => TransformEntry (Prefix symPrefix) sym sym' a a where
  transformEntry _ _ _ = identity
  untransformEntry _ _ _ = identity

---
data LowerFirst

instance LowerFirstCl sym sym' => TransformEntry LowerFirst sym sym' a a where
  transformEntry _ _ _ = identity
  untransformEntry _ _ _ = identity

---
data SingleField

instance
  ( Row.Cons symFirst a () r
  , TypeEquals symFirst "_1"
  , IsSymbol symFirst
  ) =>
  TransformEntry SingleField sym sym (Record r) a where
  transformEntry _ _ _ = Record.get (Proxy :: _ symFirst)
  untransformEntry _ _ _ x = Record.insert (Proxy :: _ symFirst) x {}

---

data ArgsToRecord :: forall k. k -> Type
data ArgsToRecord tok

instance
  ( RepRecordLike tok 1 rep r
  ) =>
  TransformEntry (ArgsToRecord tok) sym sym rep (Record r) where
  transformEntry _ _ _ = repToRecord (Proxy :: _ tok) (Proxy :: _ 1)
  untransformEntry _ _ _ = repFromRecord (Proxy :: _ tok) (Proxy :: _ 1)

---

data NoTransform

instance TransformEntry NoTransform sym sym a a where
  transformEntry _ _ _ = identity
  untransformEntry _ _ _ = identity


--- Prefix

class PrefixCl (symPrefix :: Symbol) (sym1 :: Symbol) (sym2 :: Symbol) | sym2 -> sym1

instance
  ( Sym.Cons symPrefix sym1 sym2
  ) =>
  PrefixCl symPrefix sym1 sym2

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
