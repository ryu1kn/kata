module Lib
    ( encode
    ) where

import Data.Char

encode :: Int -> String
encode x
    | x < 4 = replicate x 'I'
    | otherwise = aroundV x
    where
        aroundV x = let leftI = replicate (min (5 - x) 1) 'I'
                        rightI = replicate (max (x - 5) 0) 'I'
                    in leftI ++ "V" ++ rightI
