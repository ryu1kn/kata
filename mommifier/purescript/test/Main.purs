module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (mommify)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Mommifier" do
    it "returns as is on given empty string" do
      mommify "" `shouldEqual` ""

    it "replace a vowel (a) to mommy" do
      mommify "a" `shouldEqual` "mommy"

    it "replace a vowel (i) to mommy" do
      mommify "i" `shouldEqual` "mommy"

    it "replace a vowel (e) to mommy" do
      mommify "e" `shouldEqual` "mommy"

    it "replace a vowel (o) to mommy" do
      mommify "o" `shouldEqual` "mommy"

    it "replace a vowel (u) to mommy" do
      mommify "u" `shouldEqual` "mommy"

    it "only replaces a vowel" do
      mommify "ba" `shouldEqual` "bmommy"

    it "returns as is if vowels are less than 30% of the string" do
      mommify "bbba" `shouldEqual` "bbba"
