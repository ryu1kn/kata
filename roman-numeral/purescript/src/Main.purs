module Main where

import Prelude

import Data.List.Lazy (List, repeat, take, toUnfoldable)
import Data.String.CodeUnits (fromCharArray)

romanNumeral :: Int -> String
romanNumeral x | x < 4 = toString <<< flip take (repeat 'I') $ x
               | x == 4 = "IV"
               | x == 5 = "V"
               | x < 9 = "V" <> romanNumeral (x - 5)
               | x == 9 = "IX"
               | x == 10 = "X"
               | otherwise = "X" <> romanNumeral (x - 10)

toString :: List Char -> String
toString = toUnfoldable >>> fromCharArray
