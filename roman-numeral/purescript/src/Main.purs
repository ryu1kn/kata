module Main where

import Prelude

import Data.List.Lazy (List, repeat, take, toUnfoldable)
import Data.String.CodeUnits (fromCharArray)

type RN = { n :: Int, c :: String }

_V = {n: 5, c: "V"}
_X = {n: 10, c: "X"}
_L = {n: 50, c: "L"}

romanNumeral :: Int -> String
romanNumeral x | x < _V.n - 1 = toString <<< flip take (repeat 'I') $ x
               | x < _X.n - 1 = symbolsAround _V x
               | x < _L.n = symbolsAround _X x
               | otherwise = _L.c

symbolsAround :: RN -> Int -> String
symbolsAround rn x = subtractSymbol rn.n x <> rn.c <> romanNumeral (x - rn.n)
  where
    subtractSymbol base n = if n < base then "I" else ""

toString :: List Char -> String
toString = toUnfoldable >>> fromCharArray
