module Test.LabeledData.VariantLike.ClassSpec where

import Prelude

import Data.Either (Either(..))
import Data.Variant as V
import LabeledData.VariantLike.Class as ME
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec =
  describe "LabeledData.VariantLike.Class" do
    describe "Either" do
      it "toVariant" do
        shouldEqual
          ((Left 3 :: Either Int String) # ME.toVariant)
          (V.inj (Proxy :: _ "left") 3)

      it "fromVariant" do
        shouldEqual
          (V.inj (Proxy :: _ "left") 3 # ME.fromVariant)
          (Left 3 :: Either Int String)
