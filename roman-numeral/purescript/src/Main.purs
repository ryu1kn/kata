module Main where

import Prelude

import Data.List.Lazy (List, repeat, take, toUnfoldable)
import Data.String.CodeUnits (fromCharArray)

data RN = RN { val :: Int, face :: String, dec :: RN } | Zero

_I = RN {val: 1, face: "I", dec: Zero}
_V = RN {val: 5, face: "V", dec: _I}
_X = RN {val: 10, face: "X", dec: _I}
_L = RN {val: 50, face: "L", dec: _X}

val :: RN -> Int
val (RN r) = r.val
val Zero = 0

face :: RN -> String
face (RN r) = r.face
face Zero = ""

dec :: RN -> RN
dec (RN r) = r.dec
dec Zero = Zero

romanNumeral :: Int -> String
romanNumeral x | x < val _V - 1 = toString <<< flip take (repeat 'I') $ x
               | x < val _X - 1 = symbolsAround _V x
               | x < val _L - 10 = symbolsAround _X x
               | otherwise = symbolsAround _L x

symbolsAround :: RN -> Int -> String
symbolsAround rn x = subtractSymbol rn x <> face rn <> romanNumeral (x - val rn)
  where
    subtractSymbol base n = if n < val base then (face <<< dec) base else ""

toString :: List Char -> String
toString = toUnfoldable >>> fromCharArray
