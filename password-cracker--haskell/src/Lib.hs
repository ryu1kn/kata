module Lib
    ( decompose
    ) where

decompose :: String -> [String] -> Maybe [String]
decompose attempt [_] = Just [attempt]
