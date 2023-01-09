module LabeledData.VariantLike.Generic
  ( class ArgsRecord
  , class GenericVariantLike
  , class RepVariantLike
  , genericFromVariant
  , genericToVariant
  , repFromVariant
  , repToVariant
  , argsToRecord
  , recordToArgs
  , class LowerFirst
  , class ToLower
  , class MkIndex
  ) where

import Prelude

import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), Sum(..), from, to)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Int as Int
import Prim.Row (class Union)
import Prim.Row as Row
import Prim.Symbol as Sym
import Record as Rec
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

---

class GenericVariantLike a r | a -> r where
  genericToVariant :: a -> Variant r
  genericFromVariant :: Variant r -> a

instance
  ( Generic a rep
  , RepVariantLike rep r
  ) =>
  GenericVariantLike a r where
  genericToVariant = repToVariant <<< from
  genericFromVariant = to <<< repFromVariant

class RepVariantLike rep r | rep -> r where
  repToVariant :: rep -> Variant r
  repFromVariant :: Variant r -> rep

instance
  ( Row.Cons sym' (Record argsRec) () r
  , IsSymbol sym'
  , ArgsRecord 1 args argsRec
  , LowerFirst sym sym'
  ) =>
  RepVariantLike (Constructor sym args) r
  where
  repToVariant (Constructor args) =
    argsToRecord (Proxy :: _ 1) args
      # V.inj (Proxy :: _ sym')

  repFromVariant =
    V.case_
      # V.on (Proxy :: _ sym') (Constructor <<< recordToArgs (Proxy :: _ 1))

instance
  ( RepVariantLike b r2
  , Row.Union r2 tr r
  , LowerFirst sym sym'
  , ArgsRecord 1 args argsRec
  , Row.Cons sym' (Record argsRec) r2 r
  , IsSymbol sym'
  ) =>
  RepVariantLike (Sum (Constructor sym args) b) r
  where
  repToVariant (Inl (Constructor args)) = argsToRecord (Proxy :: _ 1) args
    # V.inj (Proxy :: _ sym')

  repToVariant (Inr x) = V.expand $ repToVariant x

  repFromVariant = matchOrContinue (Proxy :: _ sym')
    (Inl <<< Constructor <<< recordToArgs (Proxy :: _ 1))
    (Inr <<< repFromVariant)

class ArgsRecord :: Int -> Type -> Row Type -> Constraint
class ArgsRecord ix rep r | rep -> r where
  argsToRecord :: Proxy ix -> rep -> Record r
  recordToArgs :: Proxy ix -> Record r -> rep

instance ArgsRecord ix NoArguments () where
  argsToRecord _ _ = {}
  recordToArgs _ _ = NoArguments

instance
  ( ArgsRecord ix repA ra
  , ArgsRecord ix' repB rb
  , Int.Add 1 ix ix'
  , Row.Union ra rb r
  , Row.Union rb ra r
  ) =>
  ArgsRecord ix (Product repA repB) r where
  argsToRecord _ (Product a b) = Rec.union
    (argsToRecord (Proxy :: _ ix) a)
    (argsToRecord (Proxy :: _ ix') b)
  recordToArgs _ r = Product
    (recordToArgs (Proxy :: _ ix) $ pick r)
    (recordToArgs (Proxy :: _ ix') $ pick r)

instance
  ( Int.ToString ix ixs
  , MkIndex ix sym
  , Row.Cons sym a () r
  , IsSymbol sym
  ) =>
  ArgsRecord ix (Argument a) r where
  argsToRecord _ (Argument x) = Rec.insert (Proxy :: _ sym) x {}
  recordToArgs _ = Argument <<< Rec.get (Proxy :: _ sym)

--- MkIndex

class MkIndex (ix :: Int) (sym :: Symbol) | ix -> sym

instance
  ( Sym.Cons "_" ixs sym
  , Int.ToString ix ixs
  ) =>
  MkIndex ix sym

--- LowerFirst

class LowerFirst (sym1 :: Symbol) (sym2 :: Symbol) | sym1 -> sym2

instance
  ( Sym.Cons head tail sym1
  , ToLower head head'
  , Sym.Cons head' tail sym2
  ) =>
  LowerFirst sym1 sym2

-- ToLower

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

--- Internal Util

pick :: forall r2 rx r1. Union r2 rx r1 => Record r1 -> Record r2
pick = unsafeCoerce

matchOrContinue
  :: forall r r' sym a z
   . Row.Cons sym a r' r
  => IsSymbol sym
  => (Proxy sym -> (a -> z) -> (Variant r' -> z) -> Variant r -> z)
matchOrContinue sym onCase onElse v = v
  #
    ( V.default (\_ -> onElse $ unsafeCoerce v)
        # V.on sym (const <<< onCase)
    )
  # (_ $ unit)