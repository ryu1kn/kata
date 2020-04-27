module Main where

import Prelude

import Data.List (List(..), difference, fromFoldable, tail, zipWith, (:))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)

findColour :: String -> Maybe Char
findColour = deduces <<< Just <<< toCharList
  where
    toCharList = fromFoldable <<< toCharArray

deduces :: Maybe (List Char) -> Maybe Char
deduces (Just (x : Nil)) = Just x
deduces (Just list) = deduces $ deduce list
deduces Nothing = Nothing

deduce :: List Char -> Maybe (List Char)
deduce (x : Nil) = Just (x : Nil)
deduce list = map (zipWith nextColour list) (tail list)

nextColour :: Char -> Char -> Char
nextColour x y | x == y = x
               | otherwise = case difference ('R' : 'G' : 'B' : Nil) (x : y : Nil) of
                                  (z : _) -> z
                                  Nil -> 'x'
