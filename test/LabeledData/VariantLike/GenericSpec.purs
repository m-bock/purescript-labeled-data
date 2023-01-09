module Test.LabeledData.VariantLike.GenericSpec where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Variant as V
import LabeledData.VariantLike.Generic (genericFromVariant, genericToVariant)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

data Foo = Foo

data Bar = Bar Int

data Baz = Baz Int String Boolean

data Floo = Floo1 | Floo2

data Fla = Fla1 Int | Fla2 Int

data Fli = Fli1 Int String Boolean | Fli2 Int String Boolean

derive instance Generic Foo _

derive instance Generic Bar _

derive instance Generic Baz _

derive instance Generic Floo _

derive instance Generic Fla _

derive instance Generic Fli _

derive instance Eq Foo

derive instance Eq Bar

derive instance Eq Fli

derive instance Eq Baz


instance Show Foo where
  show = genericShow

instance Show Bar where
  show = genericShow

instance Show Fli where
  show = genericShow


instance Show Baz where
  show = genericShow

spec :: Spec Unit
spec =
  describe "LabeledData.VariantLike.Generic" do
    describe "GenericVariantLike" do
      describe "genericToVariant" do
        it "Single constructor, no args" do
          genericToVariant Foo
            `shouldEqual`
              (V.inj (Proxy :: _ "foo") {})

        it "Single constructor, one arg" do
          genericToVariant (Bar 2)
            `shouldEqual`
              (V.inj (Proxy :: _ "bar") { _1: 2 })

        it "Single constructor, multiple args" do
          genericToVariant (Baz 3 "" true)
            `shouldEqual`
              (V.inj (Proxy :: _ "baz") { _1: 3, _2: "", _3: true })

        it "Two constructors, each no args" do
          genericToVariant Floo1
            `shouldEqual`
              (V.inj (Proxy :: _ "floo1") {})

        it "Two constructors, each one args" do
          genericToVariant (Fla1 1)
            `shouldEqual`
              (V.inj (Proxy :: _ "fla1") { _1: 1 })

        it "Two constructors, each multiple args" do
          genericToVariant (Fli1 1 "" true)
            `shouldEqual`
              (V.inj (Proxy :: _ "fli1") { _1: 1, _2: "", _3: true })

      describe "genericFromVariant" do
        it "Single constructor, no args" do
          genericFromVariant (V.inj (Proxy :: _ "foo") {})
            `shouldEqual`
              Foo

        it "Single constructor, one arg" do
          genericFromVariant (V.inj (Proxy :: _ "bar") { _1: 2 }) 
            `shouldEqual`
              (Bar 2)

        it "Single constructor, multiple args" do
          genericFromVariant (V.inj (Proxy :: _ "baz") { _1: 3, _2: "", _3: true })
            `shouldEqual`
                (Baz 3 "" true)

        it "Two constructors, each multiple args" do
          genericFromVariant (V.inj (Proxy :: _ "fli1") { _1: 1, _2: "", _3: true })
            `shouldEqual`
              (Fli1 1 "" true)
