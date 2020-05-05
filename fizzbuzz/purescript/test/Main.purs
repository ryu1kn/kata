module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (fizzbuzz)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "FizzBuzz" do
    it "returns original number" do
      fizzbuzz 1 `shouldEqual` "1"

    it "returns Fizz" do
      fizzbuzz 3 `shouldEqual` "Fizz"

    it "returns Fizz - 6" do
      fizzbuzz 6 `shouldEqual` "Fizz"

    it "returns Buzz" do
      fizzbuzz 5 `shouldEqual` "Buzz"

    it "returns FizzBuzz for divisible by both 3 and 5" do
      fizzbuzz 15 `shouldEqual` "FizzBuzz"
