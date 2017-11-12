module Lib
    ( decompose
    ) where

import Data.List
import Data.Foldable (concatMap)

data WordTree = WordTree String [WordTree]

decompose :: String -> [String] -> Maybe [String]
decompose attempt passwords = findFullGrownBranch attempt $ buildWordTree attempt passwords

buildWordTree :: String -> [String] -> WordTree
buildWordTree attempt passwords = WordTree "" $ buildSubTreeList attempt passwords
    where
        buildSubTreeList attempt passwords =
            map (buildSubTree attempt passwords) $ filter (\x -> isPrefixOf x attempt) passwords
        buildSubTree attempt passwords pwd = WordTree pwd $ buildSubTreeList (removePrefix pwd attempt) passwords
        removePrefix prefix = drop $ length prefix

findFullGrownBranch :: String -> WordTree -> Maybe [String]
findFullGrownBranch attempt tree = find (isSameWhenJoined attempt) $ dropRootNodeFromEach $ flattenWordTree tree
    where
        flattenWordTree (WordTree password []) = [[password]]
        flattenWordTree (WordTree password subTree) = map ((:) password) (concatMap flattenWordTree subTree)
        dropRootNodeFromEach = map (drop 1)
        isSameWhenJoined fullstring strings = fullstring == intercalate "" strings
