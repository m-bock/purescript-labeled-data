module LabeledData.RecordLike.Generic
  ( class GenericRecordLike
  , class RepRecordLike
  , genericFromRecord
  , genericFromRecord'
  , genericToRecord
  , genericToRecord'
  , repFromRecord
  , repToRecord
  ) where

import Prelude

import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), from, to)
import Data.Symbol (class IsSymbol)
import LabeledData.TransformEntry (class TransformEntry, transformEntry, untransformEntry)
import Prim.Int as Int
import Prim.Row (class Union)
import Prim.Row as Row
import Record as Rec
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

--- GenericRecordLike

class GenericRecordLike :: forall k. k -> Type -> Row Type -> Constraint
class GenericRecordLike tok a r where
  genericToRecord :: Proxy tok -> a -> Record r
  genericFromRecord :: Proxy tok -> Record r -> a

instance (Generic a rep, RepRecordLike tok 1 rep r) => GenericRecordLike tok a r where
  genericToRecord prxTok = repToRecord prxTok (Proxy :: _ 1) <<< from
  genericFromRecord prxTok = to <<< repFromRecord prxTok (Proxy :: _ 1)

--- RepRecordLike

class RepRecordLike :: forall k. k -> Int -> Type -> Row Type -> Constraint
class RepRecordLike tok (ix :: Int) rep r | tok rep -> r where
  repToRecord :: Proxy tok -> Proxy ix -> rep -> Record r
  repFromRecord :: Proxy tok -> Proxy ix -> Record r -> rep

instance (RepRecordLike tok ix args r) => RepRecordLike tok ix (Constructor s args) r
  where
  repToRecord prxTok ix (Constructor args) =
    repToRecord prxTok ix args

  repFromRecord prxTok ix r =
    Constructor $ repFromRecord prxTok ix r

instance RepRecordLike tok ix NoArguments ()
  where
  repToRecord _ _ _ = {}
  repFromRecord _ _ _ = NoArguments

instance
  ( RepRecordLike tok ix a r1
  , RepRecordLike tok ix' b r2
  , Int.Add ix 1 ix'
  , Union r1 r2 r
  , Union r2 r1 r
  ) =>
  RepRecordLike tok ix (Product a b) r
  where
  repToRecord prxTok _ (Product a b) = Rec.union a' b'
    where
    a' = repToRecord prxTok (Proxy :: _ ix) a
    b' = repToRecord prxTok (Proxy :: _ ix') b

  repFromRecord prxTok _ r = Product a b
    where
    a = pick r # repFromRecord prxTok (Proxy :: _ ix)
    b = pick r # repFromRecord prxTok (Proxy :: _ ix')

instance
  ( Row.Cons sym' a' () r
  , Int.ToString ix sym
  , TransformEntry tok sym sym' a a'
  , IsSymbol sym'
  , IsSymbol sym
  ) =>
  RepRecordLike tok ix (Argument a) r
  where
  repToRecord _ _ (Argument x) =
    x
      # transformEntry (Proxy :: _ tok) (Proxy :: _ sym) (Proxy :: _ sym')
      # \x' -> Rec.insert (Proxy :: _ sym') x' {}

  repFromRecord _ _ r =
    Argument
      $ untransformEntry (Proxy :: _ tok) (Proxy :: _ sym') (Proxy :: _ sym)
      $ Rec.get (Proxy :: _ sym') r

--- Proxy API

genericToRecord' :: forall tok a r. GenericRecordLike tok a r => Proxy a -> Proxy (Record r)
genericToRecord' _ = Proxy

genericFromRecord' :: forall tok a r. GenericRecordLike tok a r => Proxy (Record r) -> Proxy a
genericFromRecord' _ = Proxy

--- Internal Util

pick :: forall r2 rx r1. Union r2 rx r1 => { | r1 } -> { | r2 }
pick = unsafeCoerce

