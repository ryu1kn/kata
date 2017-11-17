module Lib
    ( decompose
    ) where

import           Data.Foldable (concatMap)
import           Data.List

data WordTree = WordTree String [WordTree]

decompose :: String -> [String] -> Maybe [String]
decompose attempt passwords = findFullGrownBranch attempt $ buildWordTree attempt passwords

buildWordTree :: String -> [String] -> WordTree
buildWordTree attempt passwords = WordTree "" $ buildSubTreeList attempt
    where
        buildSubTreeList attempt =
            map (buildSubTree attempt) $ filter (`isPrefixOf` attempt) passwords
        buildSubTree attempt pwd = WordTree pwd $ buildSubTreeList (removePrefix pwd attempt)
        removePrefix prefix = drop $ length prefix

findFullGrownBranch :: String -> WordTree -> Maybe [String]
findFullGrownBranch attempt tree = find (isSameWhenJoined attempt) $ dropRootNodeFromEach $ flattenWordTree tree
    where
        flattenWordTree (WordTree password []) = [[password]]
        flattenWordTree (WordTree password subTree) = map ((:) password) (concatMap flattenWordTree subTree)
        dropRootNodeFromEach = map (drop 1)
        isSameWhenJoined fullstring strings = fullstring == intercalate "" strings
