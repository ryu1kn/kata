module Main where

import Prelude

import Data.List.Lazy (List, repeat, take, toUnfoldable)
import Data.String.CodeUnits (fromCharArray)

type RN = { n :: Int, c :: String }

_V = {n: 5, c: "V"}
_X = {n: 10, c: "X"}

romanNumeral :: Int -> String
romanNumeral x | x < 4 = toString <<< flip take (repeat 'I') $ x
               | x < 9 = symbolsAround _V x
               | otherwise = symbolsAround _X x

symbolsAround :: RN -> Int -> String
symbolsAround rn x = subtractSymbol rn.n x <> rn.c <> romanNumeral (x - rn.n)
  where
    subtractSymbol base n = if n < base then "I" else ""

toString :: List Char -> String
toString = toUnfoldable >>> fromCharArray
