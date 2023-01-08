module LabeledData.VariantLike.Generic where

import Prelude

import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), Sum(..), from, to)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row (class Union)
import Prim.Row as Row
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

---

class GenericVariantLike a r where
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
  ( Row.Cons s tuples () r
  , IsSymbol s
  , ArgsTuples args tuples
  ) =>
  RepVariantLike (Constructor s args) r
  where
  repToVariant (Constructor args) =
    argsToTuples args
      # V.inj (Proxy :: _ s)

  repFromVariant =
    V.case_
      # V.on (Proxy :: _ s) (Constructor <<< tuplesToArgs)

instance
  ( RepVariantLike a r1
  , RepVariantLike b r2
  , Union r1 r2 r
  , Union r2 r1 r
  ) =>
  RepVariantLike (Sum a b) r
  where
  repToVariant (Inl x) = pick' $ repToVariant x
  repToVariant (Inr x) = pick' $ repToVariant x

  repFromVariant r = repFromVariant r

---

class ArgsTuples rep a | rep -> a where
  argsToTuples :: rep -> a
  tuplesToArgs :: a -> rep

instance ArgsTuples NoArguments Unit where
  argsToTuples _ = unit
  tuplesToArgs _ = NoArguments

instance
  ( ArgsTuples repA a
  , ArgsTuples repB b
  ) =>
  ArgsTuples (Product repA repB) (Tuple a b) where
  argsToTuples (Product a b) = Tuple (argsToTuples a) (argsToTuples b)
  tuplesToArgs (Tuple a b) = Product (tuplesToArgs a) (tuplesToArgs b)

instance ArgsTuples (Argument a) a where
  argsToTuples (Argument x) = x
  tuplesToArgs = Argument

--- Internal Util

pick :: forall f r2 rx r1. Union r2 rx r1 => f r1 -> f r2
pick = unsafeCoerce

pick' :: forall f r2 rx r1. Union r2 rx r1 => f r2 -> f r1
pick' = unsafeCoerce

