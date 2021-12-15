{-# LANGUAGE NamedFieldPuns #-}

import Data.List.Split (splitOn)
import Data.Map (Map, empty, member, adjust, insert, toList)
import Control.Monad (join)

data Coordinate = Coordinate {x :: Int, y :: Int} deriving (Show)
data Line = Line {start :: Coordinate, end :: Coordinate} deriving (Show)

main :: IO ()
main = do
  input <- lines <$> getContents
  let pairs = map (splitOn " -> ") input
  let lines = map (makeLine.(map (makeCoordinate.(splitOn ",")))) pairs
  -- let filteredLines = filter horizontalOrVertical lines
  let positions = map getPositions lines -- filteredLines
  let allPositions = join positions
  let positionsWithCounts = foldl putInMap empty allPositions
  let positionsWithMoreThanOne = map fst $
        filter (\(coord, count) -> count > 1) $
        toList positionsWithCounts
  putStrLn $ show $ length positionsWithMoreThanOne
    where makeCoordinate [x,y] = Coordinate {x = read x, y = read y}
          makeLine [start, end] = Line {start = start, end = end}
          horizontal (Line {start, end}) = (y start) == (y end)
          vertical (Line {start, end}) = (x start) == (x end)
          horizontalOrVertical line = (horizontal line) || (vertical line)
          getPositions (Line {start, end})
            | (x start) == (x end) = map (\y -> (x start, y)) $ fromTo (y start) (y end)
            | (y start) == (y end) = map (\x -> (x, y start)) $ fromTo (x start) (x end)
            | otherwise = zip (fromTo (x start) (x end)) (fromTo (y start) (y end))
          fromTo x y = if (x < y) then [x..y] else reverse [y..x]
          putInMap mapSoFar newValue
            | member newValue mapSoFar = adjust (+ 1) newValue mapSoFar
            | otherwise = insert newValue 1 mapSoFar
