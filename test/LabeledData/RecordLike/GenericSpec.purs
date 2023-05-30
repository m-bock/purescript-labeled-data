module Test.LabeledData.RecordLike.GenericSpec where

import Prelude

import Data.Generic.Rep (class Generic)
import LabeledData.RecordLike.Generic (genericToRecord)
import LabeledData.TransformEntry.Transforms (Prefix)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

data Foo = Foo Int String Boolean

data Bar = Bar Int

data Baz = Baz

derive instance Generic Foo _

derive instance Generic Bar _

derive instance Generic Baz _

prxOpts =
  Proxy
    :: Proxy
         (Prefix "_")

spec :: Spec Unit
spec =
  describe "LabeledData.RecordLike.Generic" do
    describe "GenericRecordLike" do
      describe "genericToRecord" do
        it "Single constructor ADT with multiple args: returns a record with keys of the positional fields" do
          genericToRecord prxOpts (Foo 1 "" true)
            `shouldEqual`
              { _1: 1, _2: "", _3: true }
        it "Single constructor ADT with one arg: returns a record with one key" do
          genericToRecord prxOpts (Bar 1)
            `shouldEqual`
              { _1: 1 }
        it "Single constructor ADT with zero args: returns an empty record" do
          genericToRecord prxOpts Baz
            `shouldEqual`
              {}