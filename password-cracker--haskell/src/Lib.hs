module Lib
    ( decompose
    ) where

import Data.List
import Data.Foldable (concatMap)

data WordTree = WordTree String [WordTree]

decompose :: String -> [String] -> Maybe [String]
decompose attempt passwords = findFullGrownBranch attempt (buildWordTree attempt passwords)

buildWordTree :: String -> [String] -> WordTree
buildWordTree attempt passwords = WordTree "" (buildWordTree_ attempt passwords)
    where
        buildWordTree_ attempt passwords =
            let filtered = filter (\x -> isPrefixOf x attempt) passwords
            in map (\x -> WordTree x (buildWordTree_ (drop (length x) attempt) passwords)) filtered

findFullGrownBranch :: String -> WordTree -> Maybe [String]
findFullGrownBranch attempt tree =
    let noRootNode = map (drop 1) $ flattenWordTree tree
    in find (\x -> attempt == intercalate "" x) noRootNode
    where
        flattenWordTree (WordTree password []) = [[password]]
        flattenWordTree (WordTree password subTree) = map ((:) password) (concatMap flattenWordTree subTree)
