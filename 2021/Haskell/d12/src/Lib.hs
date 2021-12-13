module Lib
    ( start,
      parseInput,
      printConnections,
      findPaths
    ) where

import Data.Map (Map, fromList, empty, alter, assocs)
import Data.Set (Set, singleton, insert, elems, member)
import Data.List.Split (splitWhen)
import Data.Maybe (maybe)
import Data.Char (isLower)

type Connections = Map String (Set String)

first :: (a -> Bool) -> [a] -> a
first predicate = head.(dropWhile $ not.predicate)

start :: [SoFar]
start = [("start", ["start"], singleton "start")]

printSet :: Set String -> String
printSet s = show $ elems s

printConnections :: Connections -> String
printConnections connections = unlines $ map (\(k, v) -> (show k) ++ " : " ++ (printSet v)) $ assocs connections

makeConnections :: [(String, String)]-> Connections
makeConnections xs = foldl upsert Data.Map.empty (xs ++ map (\(a, b) -> (b, a)) xs)
  where
    upsert :: Connections -> (String, String) -> Connections
    upsert m (k, v) = alter (\mb -> Just $ maybe (singleton v) (insert v) mb) k m

splitOnDashes :: [Char] -> [String]
splitOnDashes = splitWhen (\x -> x == '-')

type SoFar = (String, [String], Set String)

type Accumulator = [(SoFar, [[String]])]

isSmall :: String -> Bool
isSmall s = isLower $ s !! 0

update :: Connections -> Accumulator -> Accumulator
update _ ([], paths) = ([], paths)
update cs ((c, path, visited):rest, paths) = do
  let potentials = elems $ cs ! c
  let realPotentials = filter (\s -> (not isSmall s) || (not member s visited)) potentials
  return ()

findPaths :: Connections -> IO [[String]]
findPaths connections = do
  return $ snd $ first (\(sofar, paths) -> null sofar) $ iterate (update connections) (start, [])

parseInput :: IO Connections
parseInput = do
  inputs <- (map splitOnDashes) <$> lines <$> getContents
  return $ makeConnections $ map (\[l,r] -> (l, r)) inputs
