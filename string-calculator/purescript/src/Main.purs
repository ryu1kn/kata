module Main where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either(..), note)
import Data.Int (fromString)
import Data.List (List(..), filter, foldM, foldl, fromFoldable, singleton, toUnfoldable, (:))
import Data.Maybe (isJust)
import Data.String (Pattern(..), drop, joinWith, split, stripPrefix)
import Data.String.CodeUnits (dropWhile, length, takeWhile)
import Data.Tuple (Tuple(..))

data Error = NotNumberError String | NegativeNumberError Int

instance errorShow :: Show Error where
  show (NotNumberError n) = show n
  show (NegativeNumberError n) = show n

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
  else bimap descError (foldl (+) 0) $ (splitToNumbers $ readInstruction s)

splitToNumbers :: Instruction -> Either (List Error) (List Int)
splitToNumbers (Instruction {delims: d, expr: e}) = foldEither $ map nonNegative $ splitWithDelims d e
  where
    nonNegative :: String -> Either Error Int
    nonNegative x = note (NotNumberError x) (fromString x) >>= assertNonNegative

assertNonNegative :: Int -> Either Error Int
assertNonNegative n = if (n >= 0) then Right n else Left $ NegativeNumberError n

splitWithDelims :: List String -> String -> List String
splitWithDelims delims input = foldM (flip splitWithDelim) input delims

splitWithDelim :: String -> String -> List String
splitWithDelim delim = fromFoldable <<< split (Pattern delim)

foldEither :: forall a b. List (Either a b) -> Either (List a) (List b)
foldEither = foldl foldEither' (Right Nil)
  where
    foldEither' es e = case Tuple es e of
                          Tuple (Left l) (Left i) -> Left $ l <> singleton i
                          Tuple l@(Left _) _ -> l
                          Tuple _ (Left i) -> Left $ singleton i
                          Tuple (Right l) (Right i) -> Right $ l <> singleton i

descError :: List Error -> String
descError = (message <> _) <<< joinListWith ", " <<< map show <<< filter isNegativeNumberError
  where
    message = "negatives not allowed: "
    joinListWith d = joinWith d <<< toUnfoldable
    isNegativeNumberError e = case e of
      NegativeNumberError _ -> true
      NotNumberError _ -> false
