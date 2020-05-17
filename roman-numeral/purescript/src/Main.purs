module Main where

import Prelude

import Data.List.Lazy (List, repeat, take, toUnfoldable)
import Data.String.CodeUnits (fromCharArray)

romanNumeral :: Int -> String
romanNumeral x | x < 4 = toString <<< flip take (repeat 'I') $ x
               | otherwise = "IV"

toString :: List Char -> String
toString = toUnfoldable >>> fromCharArray
