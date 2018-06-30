module Lib
    ( fillNumbers
    ) where

import           Data.Char

{-# ANN module "HLint: ignore Use String" #-}

type Board = [[Char]]

fillNumbers :: Board -> Board
fillNumbers board =
    [ [ countSurroundingBombs (i, j) (extendBoard board)
      | j <- [1 .. columns]
      ]
    | i <- [1 .. rows]
    ]
    where rows = length board
          columns = length (head board)

extendBoard :: Board -> Board
extendBoard b = [emptyRow]
                ++ fmap (("." ++) . (++ ".")) b
                ++ [emptyRow]
    where emptyRow = replicate (length (head b) + 2) '.'

countSurroundingBombs :: (Int, Int) -> Board -> Char
countSurroundingBombs (i, j) board
    | board !! i !! j == '*' = '*'
    | otherwise = (intToDigit . count '*' . crop (i,j)) board

crop :: (Int,Int) -> (Board -> Board)
crop (i,j) = fmap (takeAdjacent j) . takeAdjacent i

count :: Eq a => a -> ([[a]] -> Int)
count c = length . filter (== c) . concat

takeAdjacent :: Int -> ([a] -> [a])
takeAdjacent i = drop (i-1) . take (i+2)
