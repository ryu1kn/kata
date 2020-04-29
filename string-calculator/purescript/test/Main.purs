module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (stringAdd)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "String Calculator" do
    it "empty string evaluates to 0" do
      stringAdd "" `shouldEqual` Right 0

    it "one number" do
      stringAdd "1" `shouldEqual` Right 1

    it "one number" do
      stringAdd "2" `shouldEqual` Right 2

    it "multiple numbers" do
      stringAdd "1,2,3" `shouldEqual` Right 6

    it "treat newline as a delimiter as well" do
      stringAdd "1\n2" `shouldEqual` Right 3

    it "specify delimiter of choice" do
      stringAdd "//;\n1;2" `shouldEqual` Right 3

    it "negative numbers are not supported" do
      stringAdd "-1" `shouldEqual` Left "negatives not allowed"
