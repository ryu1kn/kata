module Main where

import Prelude

import Data.List (List(..), difference, fromFoldable, tail, zipWith, (:), head)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence)

data Colour = R | G | B

derive instance eqColour :: Eq Colour

instance showColour :: Show Colour where
  show R = "R"
  show G = "G"
  show B = "B"

toColour :: Char -> Maybe Colour
toColour c | c == 'R' = Just R
           | c == 'G' = Just G
           | c == 'B' = Just B
           | otherwise = Nothing

findColour :: String -> Maybe String
findColour s = map show (toColourList s >>= deduces)
  where
    toColourList :: String -> Maybe (List Colour)
    toColourList = sequence <<< map toColour <<< fromFoldable <<< toCharArray

deduces :: List Colour -> Maybe Colour
deduces (x : Nil) = Just x
deduces list = deduce list >>= deduces

deduce :: List Colour -> Maybe (List Colour)
deduce (x : Nil) = Just (x : Nil)
deduce list = map (zipWith nextColour list) (tail list)

nextColour :: Colour -> Colour -> Colour
nextColour x y | x == y = x
               | otherwise = fromMaybe R $ head $ difference (R : G : B : Nil) (x : y : Nil)
