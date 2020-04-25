module Main where

import Prelude

import Data.Array (elem, filter, length, concatMap)
import Data.Int (toNumber)
import Data.String.CodeUnits (fromCharArray, toCharArray)

mommify :: String -> String
mommify = fromCharArray <<< mommifyChars <<< toCharArray

mommifyChars :: Array Char -> Array Char
mommifyChars = applyIf shouldMommify (concatMap mommifyChar)

mommifyChar :: Char -> Array Char
mommifyChar s = if isVowel s then toCharArray "mommy" else [s]

isVowel :: Char -> Boolean
isVowel = flip elem (toCharArray "aeiou")

shouldMommify :: Array Char -> Boolean
shouldMommify = ((<) 0.3) <<< vowelRatio
  where
    vowelRatio = floatDiv <$> countVowels <*> length
    countVowels = length <<< filter isVowel

applyIf :: forall a. (a -> Boolean) -> (a -> a) -> a -> a
applyIf p f x = if p x then f x else x

floatDiv :: Int -> Int -> Number
floatDiv x y = toNumber x / toNumber y
