module Main where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)

fizzbuzz :: Int -> String
fizzbuzz x = fromMaybe (show x) $ fizzbuzz' x
  where
    fizzbuzz' = (<>) <$> watchWord 3 "Fizz" <*> watchWord 5 "Buzz"

watchWord :: Int -> String -> Int -> Maybe String
watchWord n word = \x -> if divisibleBy n x then Just word else Nothing

divisibleBy :: Int -> Int -> Boolean
divisibleBy n = \x -> x `mod` n == 0
