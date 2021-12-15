module Main where

import Lib
import Data.Map (Map, keys, elems, fromList, member, empty, alter, adjust, unionWith, (!), (!?))
import Data.List.Split (splitOn)
import Data.Maybe (maybe)

type Rules = Map String Char
type Count = Map Char Int
type Counts = Map String Count

count :: (Ord k) => [k] -> Map k Int
count xs = foldl (\m -> \k -> alter (\mb -> Just $ maybe 1 (+1) $ m !? k) k m) empty xs

first :: (a -> Bool) -> [a] -> a
first predicate = head.(dropWhile $ not.predicate)

chunks :: String -> [String]
chunks s = map (\(l,r)-> [l,r]) $ zip s (drop 1 s)

makeRule :: String -> (String, Char)
makeRule s = do
  let [l, [r]] = splitOn " -> " s
  (l, r)

replace :: Rules -> String -> String
replace rules s = do
  let replacement = rules ! s
  let [l, r] = s
  [l, replacement, r]

update :: Rules -> String -> String
update rules s = do
  let first:rest = map (replace rules) $ chunks s :: [String]
  foldr (++) "" $ first:(map tail rest)

part1 :: String -> [String] -> IO ()
part1 s ruleStrings = do
  let rules = fromList $ map makeRule ruleStrings :: Rules
  let result = head $ drop 10 $ iterate (update rules) s
  let resultCount = count result
  let counts = elems $ resultCount
  let mx = maximum counts
  let mn = minimum counts

  putStrLn $ show $ mx - mn

makeCount :: String -> (String, Map Char Int)
makeCount s = do
  let [s', [c]] = splitOn " -> " s
  let [l, r] = s'
  let n = [l, c, r]
  (s', count n)

newCount :: Rules -> Counts -> String -> Count
newCount rules counts k = do
  let replacementLetter = rules ! k
  let [l, r] = chunks $ replace rules k
  let newCount = unionWith (+) (counts ! l) (counts ! r)
  adjust (subtract 1) replacementLetter newCount


updateCounts :: Rules -> Counts -> Counts
updateCounts rules counts = do
  fromList $ map (\k -> (k, newCount rules counts k)) $ keys counts

part2 :: String -> [String] -> IO ()
part2 s ruleStrings = do
  let rules = fromList $ map makeRule ruleStrings :: Rules
  let counts = fromList $ map makeCount ruleStrings :: Counts
  let finalCounts = head $ drop 39 $ iterate (updateCounts rules) counts
  let resultCount = foldl (unionWith (+)) empty $ map (finalCounts !) $ chunks s
  let ls = length s
  let lettersOffByOne = take (ls - 2) $ drop 1 s
  let adjustedCount = foldl (\cs -> \c -> adjust (subtract 1) c cs) resultCount lettersOffByOne

  let mx = maximum adjustedCount
  let mn = minimum adjustedCount

  putStrLn $ show $ mx - mn

main :: IO ()
main = do
  s:_:ruleStrings <- lines <$> getContents
  part1 s ruleStrings
  part2 s ruleStrings
