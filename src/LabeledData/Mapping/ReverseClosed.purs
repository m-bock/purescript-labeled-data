module Heterogeneous.Mapping.ReverseClosed where

import Prelude

import Data.Either (Either(..))
import Data.Functor.App (App(..))
import Data.Functor.Variant (VariantF)
import Data.Functor.Variant as VariantF
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Prim.Row as Row
import Prim.RowList (RowList)
import Prim.RowList as RL
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class Mapping f a b | f b -> a where
  mapping :: f -> a -> b

class MappingWithIndex f i a b | f i b -> a where
  mappingWithIndex :: f -> i -> a -> b

instance Mapping (a -> b) a b where
  mapping k = k

newtype ConstMapping f = ConstMapping f

instance
  Mapping f a b =>
  MappingWithIndex (ConstMapping f) ix a b
  where
  mappingWithIndex (ConstMapping f) _ = mapping f

class HMap f a b | f b -> a where
  hmap :: f -> a -> b

class HMapWithIndex f a b | f b -> a where
  hmapWithIndex :: f -> a -> b

instance hmapRecord ::
  ( RL.RowToList rout rl
  , MapRecordWithIndex rl (ConstMapping fn) rin rout
  ) =>
  HMap fn { | rin } { | rout }
  where
  hmap = unsafeCoerce 1

instance
  ( RL.RowToList rout rl
  , MapRecordWithIndex rl fn rin rout
  ) =>
  HMapWithIndex fn { | rin } { | rout }
  where
  hmapWithIndex = Builder.build
    <<< mapRecordWithIndexBuilder (Proxy :: Proxy rl)

class MapRecordWithIndex (xs :: RowList Type) f (as :: Row Type) (bs :: Row Type) | f xs bs -> as where
  mapRecordWithIndexBuilder :: forall proxy. proxy xs -> f -> Builder { | as } { | bs }

instance mapRecordWithIndexCons ::
  ( IsSymbol sym
  , MappingWithIndex f (Proxy sym) a b
  , MapRecordWithIndex rest f as bs'
  , Row.Cons sym a bx bs'
  , Row.Cons sym b bx bs
  ) =>
  MapRecordWithIndex (RL.Cons sym b rest) f as bs
  where
  mapRecordWithIndexBuilder _ f = Builder.modify prop (mappingWithIndex f prop)
    <<< mapRecordWithIndexBuilder (Proxy :: Proxy rest) f
    where
    prop = Proxy :: Proxy sym

instance mapRecordWithIndexNil :: MapRecordWithIndex RL.Nil fn as as where
  mapRecordWithIndexBuilder _ _ = identity

-- instance hmapTuple ::
--   ( Mapping fn a a'
--   , Mapping fn b b'
--   ) =>
--   HMap fn (Tuple a b) (Tuple a' b')
--   where
--   hmap fn (Tuple a b) =
--     Tuple (mapping fn a) (mapping fn b)

-- instance hmapEither ::
--   ( Mapping fn a a'
--   , Mapping fn b b'
--   ) =>
--   HMap fn (Either a b) (Either a' b')
--   where
--   hmap fn = case _ of
--     Left a -> Left (mapping fn a)
--     Right b -> Right (mapping fn b)

instance hmapVariant ::
  ( RL.RowToList rout rl
  , MapVariantWithIndex rl (ConstMapping fn) rin rout
  ) =>
  HMap fn (Variant rin) (Variant rout)
  where
  hmap =
    mapVariantWithIndex (Proxy :: Proxy rl) <<< ConstMapping

instance hmapWithIndexVariant ::
  ( RL.RowToList rout rl
  , MapVariantWithIndex rl fn rin rout
  ) =>
  HMapWithIndex fn (Variant rin) (Variant rout)
  where
  hmapWithIndex =
    mapVariantWithIndex (Proxy :: Proxy rl)

class MapVariantWithIndex (xs :: RowList Type) f (as :: Row Type) (bs :: Row Type) | xs f bs -> as where
  mapVariantWithIndex :: forall proxy. proxy xs -> f -> Variant as -> Variant bs

instance mapVariantWithIndexCons ::
  ( IsSymbol sym
  , Row.Cons sym a r1 r2
  , Row.Cons sym b r3 r4
  , MappingWithIndex fn (Proxy sym) a b
  , MapVariantWithIndex rest fn r1 r4
  ) =>
  MapVariantWithIndex (RL.Cons sym b rest) fn r2 r4
  where
  mapVariantWithIndex _ fn =
    mapVariantWithIndex (Proxy :: Proxy rest) fn
      # Variant.on label (Variant.inj label <<< mappingWithIndex fn label)
    where
    label = Proxy :: Proxy sym

instance mapVariantWithIndexNil :: MapVariantWithIndex RL.Nil fn () r where
  mapVariantWithIndex _ _ = Variant.case_

-- instance hmapVariantF ::
--   ( RL.RowToList rin rl
--   , MapVariantFWithIndex rl (ConstMapping fn) rin rout x y
--   ) =>
--   HMap fn (VariantF rin x) (VariantF rout y)
--   where
--   hmap =
--     mapVariantFWithIndex (Proxy :: Proxy rl) <<< ConstMapping

-- instance hmapWithIndexVariantF ::
--   ( RL.RowToList rin rl
--   , MapVariantFWithIndex rl fn rin rout x y
--   ) =>
--   HMapWithIndex fn (VariantF rin x) (VariantF rout y)
--   where
--   hmapWithIndex =
--     mapVariantFWithIndex (Proxy :: Proxy rl)

-- class MapVariantFWithIndex (xs :: RowList (Type -> Type)) f (as :: Row (Type -> Type)) (bs :: Row (Type -> Type)) x y | xs f x -> as bs y where
--   mapVariantFWithIndex :: forall proxy. proxy xs -> f -> VariantF as x -> VariantF bs y

-- instance mapVariantFWithIndexCons ::
--   ( IsSymbol sym
--   , Row.Cons sym a r1 r2
--   , Row.Cons sym b r3 r4
--   , MappingWithIndex fn (Proxy sym) (a x) (b y)
--   , MapVariantFWithIndex rest fn r1 r4 x y
--   , Functor b
--   ) =>
--   MapVariantFWithIndex (RL.Cons sym a rest) fn r2 r4 x y
--   where
--   mapVariantFWithIndex _ fn =
--     mapVariantFWithIndex (Proxy :: Proxy rest) fn
--       # VariantF.on label (VariantF.inj label <<< mappingWithIndex fn label)
--     where
--     label = Proxy :: Proxy sym

-- instance mapVariantFWithIndexNil :: MapVariantFWithIndex RL.Nil fn () r x y where
--   mapVariantFWithIndex _ _ = VariantF.case_