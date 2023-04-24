module LabeledData.VariantLike.Generic
  ( argsToRecord
  , class ArgsRecord
  , class GenericVariantLike
  , class MkIndex
  , class RepVariantLike
  , genericFromVariant
  , genericFromVariant'
  , genericToVariant
  , genericToVariant'
  , recordToArgs
  , repFromVariant
  , repToVariant
  ) where

import Prelude

import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), Sum(..), from, to)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import LabeledData.VariantLike.Transform (class TransformVariantCase, transformFromVariant, transformToVariant)
import Prim.Int as Int
import Prim.Row (class Union)
import Prim.Row as Row
import Prim.Symbol as Sym
import Record as Rec
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

--- GenericVariantLike

class GenericVariantLike tok a r | a -> r where
  genericToVariant :: tok -> a -> Variant r
  genericFromVariant :: tok -> Variant r -> a

instance
  ( Generic a rep
  , RepVariantLike tok rep r
  ) =>
  GenericVariantLike tok a r where
  genericToVariant tok = repToVariant tok <<< from
  genericFromVariant tok = to <<< repFromVariant tok

--- RepVariantLike

class RepVariantLike tok rep r | rep -> r where
  repToVariant :: tok -> rep -> Variant r
  repFromVariant :: tok -> Variant r -> rep

instance
  ( Row.Cons sym' args' () r
  , IsSymbol sym'
  , ArgsRecord 1 args argsRec
  , TransformVariantCase tok sym sym' (Record argsRec) args'
  ) =>
  RepVariantLike tok (Constructor sym args) r
  where
  repToVariant _ (Constructor args) =
    argsToRecord (Proxy :: _ 1) args
      # transformToVariant (Proxy :: _ tok) (Proxy :: _ sym) (Proxy :: _ sym')
      # V.inj (Proxy :: _ sym')

  repFromVariant _ =
    V.case_
      # V.on (Proxy :: _ sym')
          ( Constructor
              <<< recordToArgs (Proxy :: _ 1)
              <<< transformFromVariant (Proxy :: _ tok) (Proxy :: _ sym') (Proxy :: _ sym)
          )

instance
  ( RepVariantLike tok b r2
  , Row.Union r2 tr r
  , ArgsRecord 1 args argsRec
  , Row.Cons sym' args' r2 r
  , IsSymbol sym'
  , TransformVariantCase tok sym sym' (Record argsRec) args'
  ) =>
  RepVariantLike tok (Sum (Constructor sym args) b) r
  where
  repToVariant tok (Inl (Constructor args)) = argsToRecord (Proxy :: _ 1) args
    # transformToVariant (Proxy :: _ tok) (Proxy :: _ sym) (Proxy :: _ sym')
    # V.inj (Proxy :: _ sym')

  repToVariant tok (Inr x) = V.expand $ repToVariant tok x

  repFromVariant tok = matchOrContinue (Proxy :: _ sym')
    (Inl <<< Constructor <<< recordToArgs (Proxy :: _ 1) <<< transformFromVariant (Proxy :: _ tok) (Proxy :: _ sym') (Proxy :: _ sym))
    (Inr <<< repFromVariant tok)

--- ArgsRecord

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

--- Proxy API

genericToVariant' :: forall tok a r. GenericVariantLike tok a r => tok -> Proxy a -> Proxy (Variant r)
genericToVariant' _ _ = Proxy

genericFromVariant' :: forall tok a r. GenericVariantLike tok a r => tok -> Proxy (Variant r) -> Proxy a
genericFromVariant' _ _ = Proxy

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