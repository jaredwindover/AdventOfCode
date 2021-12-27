module Main where

import Lib
import Control.Monad (mapM_)
import Control.Monad.Loops (iterateUntilM)
import Data.Maybe (maybe)
import Data.Map (Map, empty, alter, assocs, singleton)
import Data.Map.Internal.Debug (showTree)
import DSP.Basic (uninterleave)
import qualified Data.Map (map)

data State = State Integer Integer deriving (Eq, Show)

count :: (Ord k) => [k] -> Map k Integer
count xs = foldl (\m -> \k -> alter (\mb -> Just $ maybe 1 (+1) mb) k m) Data.Map.empty xs

first :: (a -> Bool) -> [a] -> a
first predicate = head.(dropWhile $ not.predicate)

update :: ([State], Integer, [Integer]) -> ([State], Integer, [Integer])
update ([cur@(State position score),next], count, d) = do
  let move = sum $ take 3 d
  let d' = drop 3 d
  let position' = (((position + move) - 1) `mod` 10) + 1
  let score' = score + position'
  let cur' = next
  let next' = State position' score'
  let count' = count + 3
  ([cur', next'], count', d')

type Score = Integer
type Turn = Integer
type Position = Integer

wrap :: Integer -> Integer
wrap p = ((p - 1) `mod` 10) + 1

valuesAndCounts :: [(Integer, Integer)]
valuesAndCounts = assocs $ count [a + b + c | a <- [1..3], b <- [1..3], c <- [1..3]]

isWin :: ((Position, Score), Integer) -> Bool
isWin ((_, x),_) = x >= 21

getCount :: ((Position, Score), Integer) -> Integer
getCount = snd

countTotal :: Frame -> Integer
countTotal (_, rnd) = sum $ map getCount $ assocs rnd

countWins :: Frame -> Integer
countWins (_, rnd) = sum $ map getCount $ filter isWin $ assocs rnd

countNotWins :: Frame -> Integer
countNotWins (_, rnd) = sum $ map getCount $ filter (not.isWin) $ assocs rnd

winsByTurn :: [Frame] -> [Integer]
winsByTurn = map countWins

type GameState = ((Position, Score), (Position, Score))
type Frame = (String, Map (Position, Score) Integer)

update2 :: Frame -> Frame
update2 (s, m) = do
  let m' = foldl update' empty [(value, count,position,score,mc) | (value,count)<- valuesAndCounts, ((position, score), mc) <- filter (not.isWin) $ assocs m]
  (s, m')
  where update' m' (value, count, position, score, mc) = alter update'' key' m'
          where
            position' = (wrap (position + value))
            score' = score + position'
            key' = (position', score')
            c' = mc * count
            update'' mb = Just $ maybe c' (+c') mb

diracUpdate :: [Frame] -> IO [Frame]
diracUpdate [x@(sx,_), (sy, ym)] = do
  putStrLn $ sx ++ "'s Turn"
  let x' = update2 x
  let total = countTotal x'
  let wins = countWins x'
  putStrLn $ "Wins:" ++ (show wins) ++ " Total:" ++ (show total) ++ " Res:" ++ (if (wins > 0) then show $ total `mod` wins else "N/A")
  -- putStrLn $ show y
  let ym' = if total > 0 then Data.Map.map (\v -> (v*27) - ((v*27*wins) `div` total)) ym else empty
  return [(sy, ym'), x']

doUpdate :: (Integer, [Frame], [[Frame]]) -> IO (Integer, [Frame], [[Frame]])
doUpdate (c,f,xs) = do
  f' <- diracUpdate f
  return (c + 1, f', f':xs)

part2 :: Position -> Position -> IO ()
part2 p1 p2 = do
  putStrLn $ "Part 2"
  (_,_,rnds) <- iterateUntilM (\(c,_,_) -> c == 27) doUpdate (0, [("A", singleton (p1, 0) 1), ("B", singleton (p2, 0) 1)], []) :: IO (Integer, [Frame], [[Frame]])
  let (p2rnds, p1rnds) = uninterleave $ map (!!1) rnds
  -- mapM_ (\(_, wins) -> do putStrLn $ show wins) p1rnds
  -- mapM_ (\(_, wins) -> do putStrLn $ show wins) p2rnds
  -- putStrLn $ show $ p1rnds !! 0
  -- putStrLn $ show $ p2rnds !! 1
  -- putStrLn $ show $ p1rnds !! 1
  -- putStrLn $ show $ p2rnds !! 2
  -- putStrLn $ show $ p1rnds !! 1
  -- putStrLn $ show $ p1rnds !! 2
  -- putStrLn $ show $ p2rnds !! 1
  -- putStrLn $ show $ p2rnds !! 2
  -- putStrLn $ show $ sum $ map getCount $ filter isWin $ assocs $ p1rnds !! 2
  let p1Wins = winsByTurn p1rnds
  let p2Wins = winsByTurn p2rnds
  putStrLn $ show p1Wins
  putStrLn $ show p2Wins
  putStrLn $ show $ sum p1Wins
  putStrLn $ show $ sum p2Wins
  -- putStrLn $ show $ maximum [p1Wins, p2Wins]

  -- let rnds1 = take 14 $ iterate update2 $ singleton (p1, 0) 1
  -- let rnds2 = take 14 $ iterate update2 $ singleton (p2, 0) 1
  -- let winsByTurn1 = winsByTurn rnds1
  -- let winsByTurn2 = winsByTurn rnds2
  -- let wins = zip winsByTurn1 winsByTurn2
  -- let (wl, wr):winrest = wins
  -- let x0 = (wl, wr, 1)
  -- let (_, winRatio) = foldl (\(td, xs) -> \(l, r) -> ((td - l) * (td - r) , xs ++ [(l, r, td * td)])) (1, [x0]) winrest
  -- putStrLn $ show $ winRatio
  -- putStrLn $ show rnds2
  -- let p1Counts = map (\t -> (t, g p1 21 t)) [0..15]
  -- let p2Counts = map (\t -> (t, g p2 21 t)) [0..15]
  -- putStrLn $ show p1Counts
  -- putStrLn $ show p2Counts

main :: IO ()
main = do
  ls <- map read <$> map (drop 28) <$> lines <$> getContents :: IO [Integer]
  let p1 = ls !! 0
  let p2 = ls !! 1
  -- let p1 = 4
  -- let p2 = 8

  let d = cycle [1..100]
  let rounds = iterate update ([State p1 0, State p2 0], 0, d)
  -- mapM_ (\(r,count,_) -> do putStrLn $ show r; putStrLn $ show count) $ take 10 rounds
  let (r@[State _ loserScore, _], count, _) = first (\([_, State _ score], _, _) -> score >= 1000) $ rounds
  putStrLn $ show r

  putStrLn $ show $ loserScore * count

  part2 p1 p2
