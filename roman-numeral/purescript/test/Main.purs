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

    it "4" do
      romanNumeral 4 `shouldEqual` "IV"

    it "5" do
      romanNumeral 5 `shouldEqual` "V"

    it "6" do
      romanNumeral 6 `shouldEqual` "VI"

    it "7" do
      romanNumeral 7 `shouldEqual` "VII"

    it "8" do
      romanNumeral 8 `shouldEqual` "VIII"

    it "9" do
      romanNumeral 9 `shouldEqual` "IX"

    it "10" do
      romanNumeral 10 `shouldEqual` "X"

    it "11" do
      romanNumeral 11 `shouldEqual` "XI"

    it "50" do
      romanNumeral 50 `shouldEqual` "L"

    it "40" do
      romanNumeral 40 `shouldEqual` "XL"

    it "90" do
      romanNumeral 90 `shouldEqual` "XC"

    it "198" do
      romanNumeral 198 `shouldEqual` "CXCVIII"

    it "41" do
      romanNumeral 41 `shouldEqual` "XLI"
