module Lib
    ( fillNumbers
    ) where

import Data.Char

fillNumbers :: [[Char]] -> [[Char]]
fillNumbers board =
    [
        [ countSurroundingBombs (i, j) board
            | j <- [0 .. (length $ board!!i) - 1] ]
            | i <- [0 .. length board - 1]
    ]

countSurroundingBombs :: (Int, Int) -> [[Char]] -> Char
countSurroundingBombs (i, j) board
    | board !! i !! j == '*' = '*'
    | otherwise =
        let subBoard = [ board !! x !! y | x <- [max 0 (i-1)..min (length board - 1) (i+1)],
                                           y <- [max 0 (j-1)..min ((length $ board!!x) - 1) (j+1)]
                       ]
        in toDigit . countBombs $ subBoard
    where
        countBombs = length . filter (== '*')
        toDigit = chr . (+48)
