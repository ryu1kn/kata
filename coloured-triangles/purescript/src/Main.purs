module Main where

import Prelude

import Data.List (List(..), (:), difference)
import Data.List.NonEmpty (NonEmptyList(..), fromFoldable, toUnfoldable)
import Data.Maybe (Maybe)
import Data.NonEmpty ((:|))
import Data.String.CodeUnits (fromCharArray, toCharArray)

findColour :: String -> Maybe String
findColour = (map <<< map) (fromCharList <<< findColour') toCharList
  where
    toCharList = fromFoldable <<< toCharArray
    fromCharList = fromCharArray <<< toUnfoldable

findColour' :: NonEmptyList Char -> NonEmptyList Char
findColour' (NonEmptyList (x :| (y : _))) = NonEmptyList (nextColour x y :| Nil)
findColour' (NonEmptyList (x :| _)) = NonEmptyList (x :| Nil)

nextColour :: Char -> Char -> Char
nextColour x y | x == y = x
               | otherwise = case difference ('R' : 'G' : 'B' : Nil) (x : y : Nil) of
                                (z : _) -> z
                                Nil -> 'x'
