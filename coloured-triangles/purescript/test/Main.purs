module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (findColour)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Data.Maybe (Maybe(..))

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Coloured Triangles" do
    it "no characters" do
      findColour "" `shouldEqual` Nothing

    it "single red" do
      findColour "R" `shouldEqual` Just "R"

    it "two reds" do
      findColour "RR" `shouldEqual` Just "R"

    it "suppliment missing colour code" do
      findColour "RB" `shouldEqual` Just "G"

    it "two reduction steps" do
      findColour "RRG" `shouldEqual` Just "G"
