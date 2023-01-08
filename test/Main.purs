module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.LabeledData.RecordLike.GenericSpec as Test.LabeledData.RecordLike.GenericSpec
import Test.LabeledData.VariantLike.GenericSpec as Test.LabeledData.VariantLike.GenericSpec

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  Test.LabeledData.RecordLike.GenericSpec.spec
  Test.LabeledData.VariantLike.GenericSpec.spec