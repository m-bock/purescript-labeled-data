module LabeledData.VariantLike.Util where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import Heterogeneous.Mapping.ReverseClosed (class Mapping, hmap, mappingWithIndex)
import Heterogeneous.Mapping.ReverseClosed as MC
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record as Record
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
--- WrapSingleField
--------------------------------------------------------------------------------

data WrapSingleField = WrapSingleField

instance
  ( Row.Cons sym a () r
  , TypeEquals sym "_1"
  , IsSymbol sym
  ) =>
  MC.MappingWithIndex WrapSingleField symKey a (Record r) where
  mappingWithIndex _ _ x = Record.insert (Proxy :: _ sym) x {}

wrapSingleField :: forall r1 r2. MC.HMapWithIndex WrapSingleField (Variant r1) (Variant r2) => Variant r1 -> Variant r2
wrapSingleField = MC.hmapWithIndex WrapSingleField

--------------------------------------------------------------------------------
--- UnWrapSingleField
--------------------------------------------------------------------------------

data UnWrapSingleField = UnWrapSingleField

instance

  MC.Mapping UnWrapSingleField { _1 :: a } a where
  mapping _ x = x._1

unWrapSingleField :: forall r1 r2. MC.HMap UnWrapSingleField (Variant r1) (Variant r2) => Variant r1 -> Variant r2
unWrapSingleField = MC.hmap UnWrapSingleField

--------------------------------------------------------------------------------
--- LowerFirst
--------------------------------------------------------------------------------

-- data LowerFirst = LowerFirst

-- instance
--   MC.MappingWithIndex LowerFirst { _1 :: a } a where
--   mappingWithIndex _ x = x._1

-- unWrapSingleField :: forall r1 r2. MC.HMap LowerFirst (Variant r1) (Variant r2) => Variant r1 -> Variant r2
-- unWrapSingleField = MC.hmap LowerFirst


--------------------------------------------------------------------------------
--- ?
--------------------------------------------------------------------------------

xx :: Variant (a :: { _1 :: Int })
xx = wrapSingleField (V.inj (Proxy :: _ "a") 1)

-- class C r1 r2 | r2 -> r1 where
--   wrapSingleField :: Variant r1 -> Variant r2

-- instance (RowToList r2 rl2, WrapSingleFieldRL rl2 r1 r2) => C r1 r2 where
--   wrapSingleField = unsafeCoerce 1

-- ---

-- class WrapSingleFieldRL rl2 r1 r2 | r2 rl2 -> r1 where
--   wrapSingleFieldRL :: Proxy rl2 -> Variant r1 -> Variant r2

-- instance
--   WrapSingleFieldRL (RL.Cons sym a RL.Nil) () r2 where
--   wrapSingleFieldRL _ rec = unsafeCoerce 1

--     where
--     prxSym = Proxy :: _ sym

-- else instance
--   ( WrapSingleFieldRL rl1 r1 r2'
--   , Row.Cons sym { _1 :: a } r2' r2
--   ) =>
--   WrapSingleFieldRL (RL.Cons sym a rl1) r1 r2 where
--   wrapSingleFieldRL _ = unsafeCoerce 1

-- ---

-- f' :: forall r1 r2. C r1 r2 => Proxy (Variant r1) -> Proxy (Variant r2)
-- f' _ = Proxy

-- ---

-- foo
--   :: Proxy
--        ( Variant
--            ( a ::
--                { _1 :: Int
--                }
--            )
--        )
-- foo = f' Proxy