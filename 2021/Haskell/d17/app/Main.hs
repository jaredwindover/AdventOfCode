module Main where

import Data.List.Split (splitOn)
import Lib

first :: (a -> Bool) -> [a] -> a
first predicate = head.(dropWhile $ not.predicate)

applyDrag :: Int -> Int
applyDrag x
  | x <= 0 = 0
  | otherwise = x - 1

update :: ((Int, Int),(Int, Int)) -> ((Int, Int), (Int, Int))
update ((px, py), (vx, vy)) = do
  let npx = px + vx
  let npy = py + vy
  let nvx = applyDrag vx
  let nvy = vy - 1
  ((npx, npy), (nvx, nvy))


check :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> (Bool, Bool, (Int, Int))
check ((minx, maxx), (miny, maxy)) ((px, py), (vx, vy)) = (
  minx <= px && px <= maxx && miny <= py && py <= maxy,
  px > maxx || py < miny, (px, py))

test :: ((Int, Int), (Int, Int)) -> (Int, Int) -> IO Bool
test ((minx, maxx), (miny, maxy)) (vx0, vy0) = do
  let (good,_,(px, py)) = first (\(g, b,_) -> g || b ) $ map (check ((minx, maxx), (miny, maxy))) $ iterate update ((0, 0), (vx0, vy0))
  -- putStrLn $ show (px, py)
  -- putStrLn $ show (vx0, vy0)
  return good

main :: IO ()
main = do
  [input] <- map (drop 13) <$> lines <$> getContents
  let [x, y] = map (map read) $ map (splitOn "..") $ map (drop 2) $ splitOn ", " input :: [[Int]]
  let maxy = (negate (minimum y)) - 1
  let miny = minimum y
  let maxx = maximum x
  let minx = 22
  let [tx0, tx1] = x;
  let [ty0, ty1] = y;
  let potentials = [(x, y) | x <- [minx..maxx], y <- [miny..maxy]]
  putStrLn $ show $ length potentials
  -- putStrLn $ show $ test ((0, 1), (0, 1)) (2, 2)
  results <- mapM (test ((tx0, tx1), (ty0, ty1))) potentials
  let count = length $ filter id results
  putStrLn $ show count
  -- putStrLn $ show ((minx, maxx), (miny, maxy))
