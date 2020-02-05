module Lib (createTree) where

import Data.Tree

createTree :: [Int] -> Maybe (Tree Char)
createTree [] = Nothing
createTree (x:[]) = Just $ Node {rootLabel = x, subForest = []}
createTree (x:xs) = 