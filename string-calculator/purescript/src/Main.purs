module Main where

import Prelude

import Data.Int (fromString)
import Data.List (List(..), (:), foldM, foldl, fromFoldable)
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..), drop, split, stripPrefix)
import Data.String.CodeUnits (dropWhile, length, takeWhile)
import Data.Traversable (traverse)

newtype Instruction = Instruction { delims :: List String,
                                    expr :: String }

readInstruction :: String -> Instruction
readInstruction s = if withDelimSpec s
  then Instruction { delims: (getDelim s : Nil), expr: getExpr s }
  else Instruction { delims: ("," : "\n" : Nil), expr: s }
  where
    withDelimSpec = isJust <<< stripPrefix (Pattern "//")
    getDelim = takeWhile (_ /= '\n') <<< drop 2
    getExpr = drop 1 <<< dropWhile (_ /= '\n')

stringAdd :: String -> Maybe Int
stringAdd s = if length s == 0
  then Just 0
  else foldl (+) 0 <$> (splitToNumbers $ readInstruction s)

splitToNumbers :: Instruction -> Maybe (List Int)
splitToNumbers (Instruction {delims: d, expr: e}) = traverse nonNegative $ splitWithDelims d e
  where
    nonNegative x = fromString x >>= assertNonNegative

assertNonNegative :: Int -> Maybe Int
assertNonNegative n = if (n >= 0) then Just n else Nothing

splitWithDelims :: List String -> String -> List String
splitWithDelims delims input = foldM (flip splitWithDelim) input delims

splitWithDelim :: String -> String -> List String
splitWithDelim delim = fromFoldable <<< split (Pattern delim)
