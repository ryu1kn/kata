module Main where

import Prelude

import Data.List.Lazy (find)
import Data.Maybe (fromMaybe)

data RN = RN { val :: Int, face :: String, dec :: RN } | Zero

units :: Array RN
units = [_M, _D, _C, _L, _X, _V, _I]
  where
    _I = RN {val: 1, face: "I", dec: Zero}
    _V = RN {val: 5, face: "V", dec: _I}
    _X = RN {val: 10, face: "X", dec: _I}
    _L = RN {val: 50, face: "L", dec: _X}
    _C = RN {val: 100, face: "C", dec: _X}
    _D = RN {val: 500, face: "D", dec: _C}
    _M = RN {val: 1000, face: "M", dec: _C}

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
romanNumeral x = fromMaybe "" $ flip symbolsAround x <$> maxSub
  where
    maxSub = find (\r -> lowerBound r <= x) units

lowerBound :: RN -> Int
lowerBound = (-) <$> val <*> (val <<< dec)

symbolsAround :: RN -> Int -> String
symbolsAround rn x = if x < val rn
    then (face <<< dec) rn <> face rn <> romanNumeral (x - lowerBound rn)
    else face rn <> romanNumeral (x - val rn)
