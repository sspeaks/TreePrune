module Main where

import Lib
import Data.Tree
import Data.Maybe

main :: IO ()
main = putStrLn . drawTree . fmap show . fromJust . createTree $ "HELLO"
