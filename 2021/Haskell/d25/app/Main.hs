module Main where

import Lib
import Data.Set (Set, empty, insert, member, union)
import qualified Data.Set (map)

type Point = (Int, Int)
type Vec = (Int, Int)
type Bounds = (Int, Int)

add :: Point -> Vec -> Point
add (l, r) (l', r') = (l + l', r + r')

wrap :: Bounds -> Point -> Point
wrap (bl, br) (pl, pr) = (pl `mod` bl, pr `mod` br)

move :: Bounds -> Vec -> Point -> Point
move bounds vec point = wrap bounds $ add point vec

build :: [[Char]] -> (Set Point, Set Point) -> Point -> (Set Point, Set Point)
build mat (down, right) p@(r, c) = do
  let char = (mat !! r) !! c
  let down' = if (char == 'v') then insert p down else down
  let right' = if (char == '>') then insert p right else right
  (down', right')

update' :: (Point -> Point) -> Set Point -> Point -> Point
update' mover blockers point = do
  let point' = mover point
  if (point' `member` blockers) then point else point'

update :: (Point -> Point) -> Set Point -> Set Point -> Set Point
update mover blockers points = do
  Data.Set.map (update' mover $ union blockers points) points

step :: Bounds -> (Set Point, Set Point) -> (Set Point, Set Point)
step bounds (down, right) = do
  let right' = update moveRight down right
  let down' = update moveDown right' down
  (down', right')
  where
    moveRight = move bounds (0, 1)
    moveDown = move bounds (1, 0)


showSets :: Bounds -> (Set Point, Set Point) -> String
showSets (rb, cb) (down, right) = do
  unlines $ map (map getChar) [[(r, c) | c <- [0.. (cb - 1)]] | r <- [0.. (rb - 1)]]
  where getChar p = if (p `member` down) then 'v' else if (p `member` right) then '>' else '.'

main :: IO ()
main = do
  mat <- lines <$> getContents
  let rows = length mat
  let cols = length (mat !! 0)
  let (down, right) = foldl (build mat) (empty, empty) [(r, c) | r <- [0..(rows - 1)], c <- [0..(cols - 1)]]
  let bounds = (rows, cols)
  let rounds = iterate (step bounds) (down, right)
  putStrLn $ showSets bounds $ rounds !! 0
  putStrLn $ showSets bounds $ rounds !! 1
  putStrLn $ showSets bounds $ rounds !! 2
  putStrLn $ showSets bounds $ rounds !! 58
  putStrLn $ show $ (length $ fst $ span (uncurry (/=)) $ zip rounds (drop 1 rounds)) + 1
