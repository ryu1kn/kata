module Main where

import Prelude

import Data.List.Lazy (List, repeat, take, toUnfoldable)
import Data.String.CodeUnits (fromCharArray)

romanNumeral :: Int -> String
romanNumeral = toString <<< flip take (repeat 'I')

toString :: List Char -> String
toString = toUnfoldable >>> fromCharArray
