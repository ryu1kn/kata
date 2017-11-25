module Lib
    ( price
    ) where

price :: [Int] -> Double
price [] = 0.0
price [x] = 8.0
price [x, y] | x == y    = 16.0
             | otherwise = 15.2
