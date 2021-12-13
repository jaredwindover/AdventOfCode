module Main where

import Lib

main :: IO ()
main = do
  input <- parseInput
  paths <- findPaths input
  putStrLn $ show $ paths
