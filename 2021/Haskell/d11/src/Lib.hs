module Lib
    ( Step (Update, Bump, ClearFlashed), Cell (Cell), Matrix, Accumulator
    , someFunc
    , first
    , apply
    , updateCell
    , updateAt
    , allAround
    , additionalBumps
    , makeCells
    , printCell
    , printMatrix
    , printAccumulator
    , runUntilEmpty
    , runNRounds
    , allFlashed
    , handle
    ) where

import Data.List.Index (indexed, setAt, modifyAt)
import Data.Maybe (catMaybes)
import Control.Monad (join)

data Step = Update | Bump Int Int | ClearFlashed deriving (Show)

data Cell = Cell Int Bool deriving (Show)

first :: (a -> Bool) -> [a] -> a
first predicate = head.(dropWhile $ not.predicate)

type Matrix = [[Cell]]
type Accumulator = (Matrix, [Step], Int)

printCell :: Cell -> String
printCell (Cell 0 _) = "*0*"
printCell (Cell value _)= " " ++ show value ++ " "

printMatrix :: Matrix -> String
printMatrix = unlines.(map (unwords.(map printCell)))

printAccumulator :: Accumulator -> IO ()
printAccumulator (m, s, fc) = do
  putStrLn $ printMatrix m
  putStrLn $ show s
  putStrLn $ show fc

apply :: Accumulator -> Accumulator
apply (matrix, [], flashCount) = (matrix, [], flashCount)
apply ([], stack, flashCount) = ([], [], flashCount)
apply ([[]], stack, flashCount) = ([[]], [], flashCount)
apply (matrix, Update:rest, flashCount) = (matrix, bumpValues ++ (ClearFlashed:rest), flashCount)
  where bumpValues = join $ map (\(x, row) -> map (\(y, _) -> Bump x y) $ indexed row) $ indexed matrix
apply (matrix, (ClearFlashed):rest, flashCount) = (newMatrix, rest, newFlashCount)
  where newFlashCount = foldl (\acc -> \row -> foldl (\acc -> \(Cell _ flashed) -> if flashed then acc + 1 else acc) acc row) flashCount matrix
        newMatrix = map (map clear) matrix
        clear (Cell value flashed) = Cell (if flashed then 0 else value) False
apply (matrix, (Bump x y):rest, flashCount)
  | flashed = (newMatrix, (additionalBumps x y matrix) ++ rest, flashCount)
  | otherwise = (newMatrix, rest, flashCount)
    where (newMatrix, flashed) = updateAt x y matrix

updateCell :: Cell -> (Cell, Bool)
updateCell (Cell value flashed) = (Cell newValue newFlashed,  (not flashed) && newFlashed)
  where newValue = value + 1
        newFlashed = flashed || (newValue > 9)

updateAt :: Int -> Int -> Matrix -> (Matrix, Bool)
updateAt x y matrix = do
  let row = matrix !! x
  let cell = row !! y
  let (newCell, flashed) = updateCell cell :: (Cell, Bool)
  let newMatrix = modifyAt x (setAt y newCell) matrix :: Matrix
  (newMatrix, flashed)

allAround :: [(Int, Int)]
allAround = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (-1, 1), (1, -1), (-1,-1)]

additionalBumps :: Int -> Int -> Matrix -> [Step]
additionalBumps x y matrix = map (\(x, y) -> Bump x y) $ catMaybes $ map (inBoard (x, y) matrix) allAround
  where inBoard (x, y) matrix (dx, dy)
          | (0 <= newX) && (0 <= newY) && ((length matrix) > newX) && ((length (matrix !! 0)) > newY) = Just (newX, newY)
          | otherwise = Nothing
          where newX = x + dx
                newY = y + dy

makeCells :: [String] -> [[Cell]]
makeCells = map (map (\x -> Cell (read [x]) False))

runUntilEmpty :: Matrix -> [Step] -> (Matrix, Int)
runUntilEmpty matrix stack = do
  let iterations = iterate apply (matrix, stack, 0) :: [(Matrix, [Step], Int)]
  let (m,_,flashCount) = first (\(_, stack, _) -> null stack) iterations
  (m, flashCount)

runNRounds :: Int -> Matrix -> (Matrix, Int)
runNRounds n matrix = do
  let stack = take n $ repeat Update
  runUntilEmpty matrix stack

allFlashed :: Matrix -> Bool
allFlashed m = all (\(Cell value flashed) -> value == 0) $ join m

run1Round :: Matrix -> Matrix
run1Round matrix = do
  fst $ runUntilEmpty matrix [Update]

handle :: Matrix -> IO ()
handle matrix = do
  let iterations = iterate run1Round (matrix) :: [Matrix]
  let (n, _) = first (\(c, m) -> allFlashed m)  $ indexed iterations
  putStrLn $ show $ n

someFunc :: IO ()
someFunc = do
  matrix <- makeCells <$> lines <$> getContents :: IO [[Cell]]
  handle matrix
