module Main where

import Prelude

import Data.Either (Either(..), note)
import Data.Int (fromString)
import Data.List (List(..), (:), foldM, foldl, fromFoldable)
import Data.Maybe (isJust)
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

stringAdd :: String -> Either String Int
stringAdd s = if length s == 0
  then Right 0
  else foldl (+) 0 <$> (splitToNumbers $ readInstruction s)

splitToNumbers :: Instruction -> Either String (List Int)
splitToNumbers (Instruction {delims: d, expr: e}) = traverse nonNegative $ splitWithDelims d e
  where
    nonNegative x = note "not a number" (fromString x) >>= assertNonNegative

assertNonNegative :: Int -> Either String Int
assertNonNegative n = if (n >= 0) then Right n else Left "negatives not allowed"

splitWithDelims :: List String -> String -> List String
splitWithDelims delims input = foldM (flip splitWithDelim) input delims

splitWithDelim :: String -> String -> List String
splitWithDelim delim = fromFoldable <<< split (Pattern delim)
