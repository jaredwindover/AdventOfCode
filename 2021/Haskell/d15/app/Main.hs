module Main where

import Prelude hiding (lookup)
import Data.Map (Map, alter, empty, (!?), fromList)
import Data.Maybe (maybe, catMaybes, fromMaybe)
import Data.SortedList (SortedList, toSortedList, insert, uncons, take)
import Lib

type Matrix = [[Int]]
type Cell = (Int, Int)

allAround :: [(Int, Int)]
allAround = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (-1, 1), (1, -1), (-1,-1)]

fourAround :: [(Int, Int)]
fourAround = [(0, 1), (0, -1), (1, 0), (-1, 0)]

aroundCellInMatrix :: [(Int, Int)] -> Int -> Int -> (Int, Int) -> [(Int, Int)]
aroundCellInMatrix ds xmax ymax (px, py) = filter inside ns
  where
    ns = map (\(dx, dy) -> (px + dx, py + dy)) ds
    inside = (\(nx, ny) -> (0 <= nx) && (0 <= ny) && (nx < xmax) && (ny < ymax))

nextFour :: Int -> Int -> (Int, Int) -> [(Int, Int)]
nextFour = aroundCellInMatrix fourAround

count :: (Ord k) => [k] -> Map k Int
count xs = foldl (\m -> \k -> alter (\mb -> Just $ maybe 1 (+1) mb) k m) empty xs

first :: (a -> Bool) -> [a] -> a
first predicate = head.(dropWhile $ not.predicate)

charToString :: Char -> String
charToString x = [x]

readMatrix :: IO Matrix
readMatrix = do
  map (map (read.charToString)) <$> lines <$> getContents

type Risk = Int
data Frame = Frame Cell Risk deriving (Eq, Show)
instance Ord Frame where
  compare (Frame _ risk1) (Frame _ risk2) = compare risk1 risk2
data CleverMatrix = CleverMatrix Matrix Int Int deriving (Eq, Show)

lookup :: CleverMatrix -> Cell -> Risk
lookup (CleverMatrix m _ _) (x, y) = do
  let origXMax = length m
  let origYMax = length (m !! 0)
  let origX = mod x (origXMax)
  let origY = mod y (origYMax)
  let incX = div x (origXMax)
  let incY = div y (origYMax)
  let origRisk = (m !! origX) !! origY
  let newRisk = (mod (origRisk + incX + incY - 1) 9) + 1
  newRisk

update :: (CleverMatrix, Cell) -> (SortedList Frame, Map Cell Risk, Bool, Int)
  -> (SortedList Frame, Map Cell Risk, Bool, Int)
update (m, d) (fs, mp, True, val) = (fs, mp, True, val)
update (CleverMatrix m xMax yMax, d) (fs, mp, False, val) = handle $ uncons fs
  where
    nextCells = nextFour xMax yMax
    handle Nothing = (fs, mp, True, val)
    handle (Just ((Frame (r,c) soFar), rest)) = do
      if ((r, c) == d) then (fs, mp, True, soFar') else (newfs, newMp, False, val)
      where
        soFar' = soFar + (lookup (CleverMatrix m xMax yMax) (r, c))
        potentialFs = filter (\(Frame cell risk) -> fromMaybe True $ fmap (\risk' -> risk < risk') $ mp !? cell)
          $ map (\cell ->  Frame cell soFar')
          $ nextCells (r, c)
        newfs = foldl (flip insert) rest potentialFs
        newMp = foldl (\mp' -> \(Frame cell risk) -> alter (\mb -> Just $ maybe risk (\v -> min v risk) mb ) cell mp') mp potentialFs

inspect :: (SortedList Frame, Map Cell Risk, Bool, Int)
  -> IO (SortedList Frame, Map Cell Risk, Bool, Int)
inspect (slfs, mp, b, i) = do
  putStrLn $ show $ Data.SortedList.take 10 slfs
  return (slfs, mp, b, i)

run :: Matrix -> IO ()
run matrix = do
  let lastRow = ((length matrix) * 5) - 1
  let lastCol = ((length (matrix !! 0)) * 5) - 1
  let destination = (lastRow, lastCol)
  let cleverMatrix = CleverMatrix matrix (lastRow + 1) (lastCol + 1)

  let (_, _, _, totalRisk) = first (\(_, _, finished, _) -> finished) $ iterate (update (cleverMatrix, destination)) (toSortedList [Frame (0, 0) 0], fromList [((0,0), 0)], False, -1)
  putStrLn $ show $ totalRisk - (matrix !! 0) !! 0


main :: IO ()
main = do
  matrix <- readMatrix
  run matrix
