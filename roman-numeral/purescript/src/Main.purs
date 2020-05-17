module Main where

import Prelude

import Data.List.Lazy (List, repeat, take, toUnfoldable)
import Data.String.CodeUnits (fromCharArray)

romanNumeral :: Int -> String
romanNumeral x | x < 4 = toString <<< flip take (repeat 'I') $ x
               | x < 9 = subtractSymbol 5 x <> "V" <> romanNumeral (x - 5)
               | otherwise = subtractSymbol 10 x <> "X" <> romanNumeral (x - 10)

subtractSymbol :: Int -> Int -> String
subtractSymbol base x = if x < base then "I" else ""

toString :: List Char -> String
toString = toUnfoldable >>> fromCharArray
