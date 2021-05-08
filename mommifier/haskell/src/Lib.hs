module Lib
    ( mommify
    ) where

mommify :: String -> String
mommify s = concat $ map mommifyChar s

mommifyChar :: Char -> String
mommifyChar c = if isVowel c then "mommy" else (c : "")

isVowel :: Char -> Bool
isVowel = flip elem $ "aeiou"
