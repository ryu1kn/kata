module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (romanNumeral)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Roman numeral" do
    it "1" do
      romanNumeral 1 `shouldEqual` "I"

    it "2" do
      romanNumeral 2 `shouldEqual` "II"

    it "3" do
      romanNumeral 3 `shouldEqual` "III"
