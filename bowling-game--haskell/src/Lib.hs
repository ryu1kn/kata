module Lib
    ( score
    ) where

score :: [Int] -> Int
score [] = 0
score (x1:x2:x3:xs)
    | isStrike x1   = 10 + x2 + x3 + score (x2:x3:xs)
    | isSpare x1 x2 = 10 + x3 + score (x3:xs)
    | otherwise     = x1 + score (x2:x3:xs)
score (x1:x2:xs)
    | isStrike x1 = 10 + x2 + score (x2:xs)
    | otherwise   = x1 + score (x2:xs)
score (x:xs) = x + score xs

isStrike :: Int -> Bool
isStrike x = x == 10

isSpare :: Int -> Int -> Bool
isSpare x y = x + y == 10
