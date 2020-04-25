module Main where

import Prelude

import Data.Array (elem, filter, length) as A
import Data.String (Pattern(..), split, joinWith, length)
import Data.Int (toNumber)

mommify :: String -> String
mommify = applyIf shouldMommify (joinWith "" <<< map mommify' <<< chars)

applyIf :: forall a. (a -> Boolean) -> (a -> a) -> a -> a
applyIf p f x = if p x then f x else x

mommify' :: String -> String
mommify' s = if isVowel s then "mommy" else s

isVowel :: String -> Boolean
isVowel c = A.elem c ["a", "e", "i", "o", "u"]

shouldMommify :: String -> Boolean
shouldMommify = ((<) 0.3) <<< vowelRatio
  where
    vowelRatio :: String -> Number
    vowelRatio = floatDiv <$> countVowels <*> length

    countVowels :: String -> Int
    countVowels = A.length <<< A.filter isVowel <<< chars

floatDiv :: Int -> Int -> Number
floatDiv x y = toNumber x / toNumber y

chars :: String -> Array String
chars = split (Pattern "")
