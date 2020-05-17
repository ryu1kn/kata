module Main where

import Prelude

import Data.List.Lazy (List, repeat, take, toUnfoldable)
import Data.String.CodeUnits (fromCharArray)

romanNumeral :: Int -> String
romanNumeral x | x < 4 = toString <<< flip take (repeat 'I') $ x
               | x < 9 = symbolsAround 5 "V" x
               | otherwise = symbolsAround 10 "X" x

symbolsAround :: Int -> String -> Int -> String
symbolsAround b bc x = subtractSymbol b x <> bc <> romanNumeral (x - b)
  where
    subtractSymbol base n = if n < base then "I" else ""

toString :: List Char -> String
toString = toUnfoldable >>> fromCharArray
