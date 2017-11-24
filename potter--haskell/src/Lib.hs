module Lib
    ( price
    ) where

price :: [Int] -> Double
price x = (* 8.0) $ fromIntegral $ length x
