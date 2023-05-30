module LabeledData.VariantLike.Generic
  ( class GenericVariantLike
  , class RepVariantLike
  , genericFromVariant
  , genericFromVariant'
  , genericToVariant
  , genericToVariant'
  , repFromVariant
  , repToVariant
  ) where

import Prelude

import Data.Generic.Rep (class Generic, Constructor(..), Sum(..), from, to)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import LabeledData.TransformEntry (class TransformEntry, untransformEntry, transformEntry)
import Prim.Row as Row
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

--- GenericVariantLike

class GenericVariantLike :: forall k. k -> Type -> Row Type -> Constraint
class GenericVariantLike tok a r | a -> r where
  genericToVariant :: Proxy tok -> a -> Variant r
  genericFromVariant :: Proxy tok -> Variant r -> a

instance
  ( Generic a rep
  , RepVariantLike tok rep r
  ) =>
  GenericVariantLike tok a r where
  genericToVariant tok = repToVariant tok <<< from
  genericFromVariant tok = to <<< repFromVariant tok

--- RepVariantLike

class RepVariantLike :: forall k. k -> Type -> Row Type -> Constraint
class RepVariantLike tok rep r | rep -> r where
  repToVariant :: Proxy tok -> rep -> Variant r
  repFromVariant :: Proxy tok -> Variant r -> rep

instance
  ( Row.Cons sym' args' () r
  , IsSymbol sym'
  , TransformEntry tok sym sym' args args'
  ) =>
  RepVariantLike tok (Constructor sym args) r
  where
  repToVariant _ (Constructor args) =
    args
      # transformEntry (Proxy :: _ tok) (Proxy :: _ sym) (Proxy :: _ sym')
      # V.inj (Proxy :: _ sym')

  repFromVariant _ =
    V.case_
      # V.on (Proxy :: _ sym')
          ( Constructor
              <<< untransformEntry (Proxy :: _ tok) (Proxy :: _ sym') (Proxy :: _ sym)
          )

instance
  ( RepVariantLike tok b r2
  , Row.Union r2 tr r
  , Row.Cons sym' args' r2 r
  , IsSymbol sym'
  , TransformEntry tok sym sym' args args'
  ) =>
  RepVariantLike tok (Sum (Constructor sym args) b) r
  where
  repToVariant prxTok (Inl (Constructor args)) = args
    # transformEntry prxTok (Proxy :: _ sym) (Proxy :: _ sym')
    # V.inj (Proxy :: _ sym')

  repToVariant tok (Inr x) = V.expand $ repToVariant tok x

  repFromVariant tok = matchOrContinue (Proxy :: _ sym')
    (Inl <<< Constructor <<< untransformEntry (Proxy :: _ tok) (Proxy :: _ sym') (Proxy :: _ sym))
    (Inr <<< repFromVariant tok)

--- Proxy API

genericToVariant' :: forall tok a r. GenericVariantLike tok a r => tok -> Proxy a -> Proxy (Variant r)
genericToVariant' _ _ = Proxy

genericFromVariant' :: forall tok a r. GenericVariantLike tok a r => tok -> Proxy (Variant r) -> Proxy a
genericFromVariant' _ _ = Proxy

--- Internal Util

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