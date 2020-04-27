module Main where

import Prelude

import Data.List (List(..), (:), difference)
import Data.List.NonEmpty (NonEmptyList(..), fromFoldable)
import Data.Maybe (Maybe)
import Data.NonEmpty ((:|))
import Data.String.CodeUnits (toCharArray)

findColour :: String -> Maybe Char
findColour = (map <<< map) findColour' toCharList
  where
    toCharList = fromFoldable <<< toCharArray

findColour' :: NonEmptyList Char -> Char
findColour' (NonEmptyList (x :| (y : _))) = nextColour x y
findColour' (NonEmptyList (x :| _)) = x

nextColour :: Char -> Char -> Char
nextColour x y | x == y = x
               | otherwise = case difference ('R' : 'G' : 'B' : Nil) (x : y : Nil) of
                                (z : _) -> z
                                Nil -> 'x'
