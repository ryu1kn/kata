module Lib
    ( encode
    ) where

import Data.Char

encode :: Int -> String
encode x
    | x < 4 = take x $ repeat 'I'
    | otherwise = aroundV x
    where
        aroundV x = let leftI = take (min (5 - x) 1) $ repeat 'I'
                        rightI = take (max (x - 5) 0) $ repeat 'I'
                    in leftI ++ "V" ++ rightI
