module Main where

import Prelude

import Data.Int (toNumber)
import Data.List ((:), elem, List(..), filter, fromFoldable, toUnfoldable, singleton, length, concatMap)
import Data.String.CodeUnits (fromCharArray, toCharArray)

mommify :: String -> String
mommify = fromCharList <<< mommifyChars <<< toCharList

mommifyChars :: List Char -> List Char
mommifyChars = applyIf shouldMommify (concatMap mommifyChar <<< squashVowels)

squashVowels :: List Char -> List Char
squashVowels (c1 : c2 : list) | isVowel c1 && isVowel c2 = squashVowels (c1 : list)
squashVowels (c : list)                                  = c : squashVowels list
squashVowels Nil                                         = Nil

mommifyChar :: Char -> List Char
mommifyChar s = if isVowel s then toCharList "mommy" else singleton s

isVowel :: Char -> Boolean
isVowel = flip elem (toCharList "aeiou")

shouldMommify :: List Char -> Boolean
shouldMommify = ((<) 0.3) <<< vowelRatio
  where
    vowelRatio = floatDiv <$> countVowels <*> length
    countVowels = length <<< filter isVowel

applyIf :: forall a. (a -> Boolean) -> (a -> a) -> a -> a
applyIf p f x = if p x then f x else x

floatDiv :: Int -> Int -> Number
floatDiv x y = toNumber x / toNumber y

toCharList :: String -> List Char
toCharList = fromFoldable <<< toCharArray

fromCharList :: List Char -> String
fromCharList = fromCharArray <<< toUnfoldable
