module Lib
    ( mommify
    ) where

mommify :: String -> String
mommify = concatMap mommifyChar

mommifyChar :: Char -> String
mommifyChar c = if isVowel c then "mommy" else c : ""

isVowel :: Char -> Bool
isVowel = flip elem "aeiou"
