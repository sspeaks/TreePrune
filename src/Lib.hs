module Lib (pruneNumbers) where

import           Data.Function
import           Data.List
import           Data.Tree

pruneNumbers = treeToStrings . pruneTree . createTree
---------------------------------------------------------------------------------------------------------------------------------
createTree :: [Int] -> Tree Char
createTree xs =
        let strings = map show xs
            maxLength = maximum . map length $ strings
            paddedStrings = map (\s -> (replicate (maxLength - length s) '0') ++ s) strings
            trees = createSingleTree <$> paddedStrings
        in Node '0' $ combineTrees trees
        where
            combineTrees :: (Ord a, Eq a) => [Tree a] -> [Tree a]
            combineTrees [] = []
            combineTrees [t] = [t]
            combineTrees trees
                | aRoot == bRoot = combineTrees ((Node aRoot $ combineTrees (subForest a ++ subForest b)):xs)
                | otherwise = a:combineTrees (b:xs)
                where
                    (a:b:xs) = sortBy (compare `on` rootLabel) trees
                    aRoot = rootLabel a
                    bRoot = rootLabel b
            createSingleTree :: String -> Tree Char
            createSingleTree  = unfoldTree fromInt
            fromInt :: String -> (Char, [String])
            fromInt (x:[]) = (x, [])
            fromInt (x:xs) = (x, [xs])
---------------------------------------------------------------------------------------------------------------------------------
treeToStrings :: Tree Char -> [String]
treeToStrings (Node a []) = []
treeToStrings t = getLeftMost t:(treeToStrings . pruneLeftMost $ t)
        where
              getLeftMost (Node x [])     =  x:[]
              getLeftMost (Node x (a:xs)) = x:getLeftMost a
              pruneLeftMost t@(Node _ []) = t
              pruneLeftMost t@(Node a (b:xs)) = 
                  if (length . subForest . pruneLeftMost $ b) == 0 
                      then Node a xs 
                      else Node a (pruneLeftMost b:xs)
---------------------------------------------------------------------------------------------------------------------------------
pruneTree :: Tree a -> Tree a
pruneTree (Node a b) =
    let children = map pruneTree b
        hasAllChildren = length children == 10
        childrenAreEmpty = (sum . map (length . subForest) $ children) == 0
    in if hasAllChildren && childrenAreEmpty 
        then Node a [] 
        else Node a children
