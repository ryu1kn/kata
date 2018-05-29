module Lib
    ( fillNumbers
    ) where

import           Data.Char

{-# ANN module "HLint: ignore Use String" #-}

type Board = [[Char]]

fillNumbers :: Board -> Board
fillNumbers board =
    [ [ countSurroundingBombs (i, j) board
      | j <- [0 .. length (board!!i) - 1]
      ]
    | i <- [0 .. length board - 1]
    ]

countSurroundingBombs :: (Int, Int) -> Board -> Char
countSurroundingBombs (i, j) board
    | board !! i !! j == '*' = '*'
    | otherwise = (intToDigit . count '*' . crop (i,j)) board

crop :: (Int,Int) -> (Board -> Board)
crop (i,j) b = let (m,n) = size b
               in (fmap (takeAdjacent j n) . takeAdjacent i m) b

count :: Eq a => a -> ([[a]] -> Int)
count c = length . filter (== c) . concat

takeAdjacent :: Int -> Int -> ([a] -> [a])
takeAdjacent i m = drop low . take high
  where low  = max 0 (i-1)
        high = min m (i+2)

size :: [[a]] -> (Int,Int)
size b = (length b, length (head b))
