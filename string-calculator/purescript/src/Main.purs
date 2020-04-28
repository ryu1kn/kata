module Main where

import Prelude

import Data.Int (fromString)
import Data.List (List(..), (:), foldM, foldl, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.String (split, Pattern(..))
import Data.String.CodeUnits (length)
import Data.Traversable (traverse)

stringAdd :: String -> Maybe Int
stringAdd s = if length s == 0
  then Just 0
  else map addNumbers $ splitToNumbers s

addNumbers :: List Int -> Int
addNumbers list = foldl (+) 0 list

splitToNumbers :: String -> Maybe (List Int)
splitToNumbers = traverse fromString <<< splitWithDelims ("," : "\n" : Nil)

splitWithDelims :: List String -> String -> List String
splitWithDelims delims input = foldM (flip splitWithDelim) input delims

splitWithDelim :: String -> String -> List String
splitWithDelim delim = fromFoldable <<< split (Pattern delim)
