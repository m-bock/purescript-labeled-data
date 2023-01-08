-- # purescript-labled-data

-- # Imports
-- For the code samples in this file you need the following imports:

module Test.GenReadme where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Data.Variant as V
import Type.Proxy (Proxy(..))

-- 
-- ## ADTs: Sums and Products
-- 
-- PureScript has Haskell-like algebraic data types (ADTs). They can be
-- described as tagged union types with positional data fields.
--
-- The following ADT definition is indented in a way that highlights the two
-- main properties of such a type.
-- 

data Foo
  -- Constructor    Arg 1                  Arg 2                  Arg 3
  = MakeA Int String (Maybe Int)
  | MakeB Boolean
  | MakeC
  | MakeD { x :: Int, y :: Int }
  | MakeE Char { x :: Int, y :: Int }

-- - On the vertical axis alternatives to
--   construct a value of this type are listed. They're distinguished by the name
--   of the constructor also known as it's label. This is an "OR" relation.
-- - On the horizontal axis describes the positional fields that each
--   constructor can hold. This is an "AND" relation.
--
-- Those types are called algebraic because if you know the number of possible
-- inhabitants of each field, you can calculate the number of inhabitants of an
-- ADT by this formula: Make the product for each field per constructor and take
-- the sum of the results.
--
-- PureScript provides very convenient ways to deal with ADTs. They can be
-- constructed with their constructors:

foo1 :: Foo
foo1 = MakeA 3 "bla" (Just 1)

foo2 :: Foo
foo2 = MakeC

foo3 :: Foo
foo3 = MakeE 'a' { x: 3, y: 2 }

-- And they can be destructed by their constructors as well:

f :: Foo -> String
f foo = case foo of
  MakeA _ str _ -> str
  MakeB _ -> "B"
  MakeC -> "C"
  MakeD _ -> "D"
  MakeE _ _ -> "E"

-- ## Records and Variants. A better alternative?
--
-- One downside of ADTs is that they can only be treated as a whole. The number
-- of cases is fixed and the number of fields is also fixed. There is no way to
-- compose them from smaller units. Nor can you narrow down a type to another
-- type with less constructor cases. The same is true for the fields in each
-- case. For the latter PureScript provides already a powerful alternative with
-- great builtin support: Records.
-- The fields of the `MakeA` constructor could also be described as `{ foo ::
-- Int, bar :: String, baz :: Maybe Int }`
-- This is syntactic sugar for `Record (foo :: Int, bar :: String, baz :: Maybe
-- Int)`. PureScript provides a special kind called "Row" which can be regarded
-- as a labeled collection of types. The `Record` type constructor takes a `Row`
-- of types to create a record type. Values of this type must then provide all
-- specified fields. Thus Records can be regarded as a sort of Product type with
-- labeled fields. Records are very flexible because there is a lot of type
-- level programming operating on `Rows` possible.
-- 
-- Unfortunately there is no builtin equivalent for Sum types. But fortunately
-- PureScript is flexible enough that something like this can be provided as
-- a library:
-- [purescript-variant](https://github.com/natefaubion/purescript-variant)
-- If you're not familiar with `Varaint` it's recommended to go through the
-- library's README.
-- 
-- Now we're able to redefine the above ADT as a Variant type:

type Vec = { x :: Int, y :: Int }

type FooV = Variant
  ( makeA :: Record (_1 :: Int, _2 :: String, _3 :: Boolean)
  , makeB :: Record (_1 :: Boolean)
  , makeC :: Record ()
  , makeD :: Record (_1 :: Vec)
  , makeE :: Record (_1 :: Char, _2 :: Vec)
  )

-- The fields for each case are defined as records. To highlight the analogy the
-- more verbose `Record` syntax is used.
-- Variants are structurally equivalent to Records sharing the same type level
-- advantages of Rows. The difference is just that the fields are interpreted as
-- cases.
--
-- So if Variants and Records are more powerful, why not using them as a
-- replacement of ADTs? Theoretically it would be possible to avoid ADTs
-- completely. With Variants wrapped in a newtype even recursive data structures
-- like `List` are possible. The main problem why `Variants` are not as
-- convenient is that they are not builtin into the language. Maybe not yet.
-- This makes constructing ang pattern matching look very noisy.


-- <table>
-- <tr><td></td><td>Existing Syntax<td></td><td>Proposed Syntax</td></tr>
-- <tr>
-- <td>Type Definition</td>
-- <td>
--
-- ```text
-- type MyVar = Variant
--   ( a :: Int
--   , b :: String
--   , c :: Boolean
--   )
-- ```
-- <td>
-- <td>
--
-- ```text
-- type MyVar = 
--   { a :: Int
--   | b :: String
--   | c :: Boolean
--   }
-- ```
-- <td>
-- </tr>
-- <tr>
-- <td>Constructing</td>
-- <td>
--
-- ```text
-- myVar1 :: MyVar
-- myVar1 = V.inj (Proxy :: _ "a") 34
-- ```
-- <td>
-- <td>
--
-- ```text
-- myVar1 :: MyVar
-- myVar1 = |a| 34
-- ```
-- <td>
-- </tr>
-- <tr>
-- <td>Pattern matching</td>
-- <td>
--
-- ```text
-- toInt :: MyVar -> Int
-- toInt = V.case_ # V.onMatch
--   { a: \_ -> 0
--   , b: \_ -> 1
--   }
-- ```
-- <td>
-- <td>
--
-- ```text
-- toInt :: MyVar -> Int
-- toInt = case_ of
--   a _ -> 0
--   b _ -> 1
-- ```
-- <td>
-- </tr>
-- <table>
--
-- Another advantage of ADTs might be that positional fields are sometimes a bit
-- cleaner in cases where the meaning is obvious,
--
-- In any way, in the current state of the language for me ADTs are still the
-- most concise way to describe data. So for good reasons you don't want to
-- define your type as a Variant, however there may be places in your program
-- where it would be quite convenient to convert your type to a Variant.
-- 
-- Let's look at an example case where we make use of this library.

derive instance Generic Foo _