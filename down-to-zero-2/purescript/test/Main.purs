module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (downToZero)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Down to Zero II" do
    it "N = 0" do
      downToZero 0 `shouldEqual` 0

    it "N = 1" do
      downToZero 1 `shouldEqual` 1
