module Main where

import Lib
import Data.List.Split (splitOn)
import Data.Set (fromList, elems, member)
import Control.Monad

data Fold = X Int | Y Int deriving (Show)
type Coord = (Int, Int)

makeFold :: (String, Int) -> Fold
makeFold ("x", value) = X value
makeFold (_, value) = Y value

foldCoord :: Fold -> Coord -> Coord
foldCoord (X value) (x, y)
  | x < value = (x, y)
  | otherwise = ((2*value) - x, y)
foldCoord (Y value) (x, y)
  | y < value = (x, y)
  | otherwise = (x, (2*value) - y)


applyFold :: [Coord] -> Fold -> [Coord]
applyFold coords fold = elems $ fromList $ map (foldCoord fold) coords

main :: IO ()
main = do
  (rawCoordinates, _:rawFolds) <- span (elem ',') <$> lines <$> getContents
  let coords = map ((\[l, r] -> (l, r)).(map read).(splitOn ",")) rawCoordinates :: [(Int, Int)]
  let folds = map (makeFold.(\[l, r] -> (l, read r)).(splitOn "=").(drop 11)) rawFolds
  let answer = foldl applyFold coords folds
  let minX = minimum $ map fst answer
  let maxX = maximum $ map fst answer
  let minY = minimum $ map snd answer
  let maxY = maximum $ map snd answer
  let answerSet = fromList answer
  let grid = map (\y -> map (\x -> (x, y)) [minX..maxX]) [minY..maxY]
  let out = unlines $ map (\r ->  map (\p -> if member p answerSet then '#' else ' ') r) grid

  putStrLn out
