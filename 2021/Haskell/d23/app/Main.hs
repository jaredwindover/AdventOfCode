module Main where

import Lib
import Control.Monad (join, mapM)
import Control.Monad.Loops (iterateUntilM)
import Data.Map (Map, fromList, member, empty, alter, insert, delete, findWithDefault, (!), (!?))
import Data.Set (Set, elems)
import qualified Data.Set (fromList, delete, insert, member, empty)
import Data.SortedList (SortedList, singleton, uncons, toSortedList)
import qualified Data.SortedList (insert)
import Data.Maybe (maybe, isJust, fromJust)
import Data.Ord (Ordering (EQ))

data Color = A | B | C | D deriving (Eq, Ord, Show)

instance Read Color where
  readsPrec x ('A':xs) = [(A, xs)]
  readsPrec x ('B':xs) = [(B, xs)]
  readsPrec x ('C':xs) = [(C, xs)]
  readsPrec x ('D':xs) = [(D, xs)]

type Point = (Int, Int)

hallwaySpots :: [Point]
hallwaySpots = [
  (1, 1),
  (1, 2),
  (1, 4),
  (1, 6),
  (1, 8),
  (1, 10),
  (1, 11)
  ]

cellSpots :: [Point]
cellSpots = [
  (2, 3), (3, 3),
  (2, 5), (3, 5),
  (2, 7), (3, 7),
  (2, 9), (3, 9)
  ]

inHall :: Point -> Bool
inHall x = elem x hallwaySpots

inCell :: Point -> Bool
inCell x = elem x cellSpots

aInHall :: Amphipod -> Bool
aInHall (Amph _ p)= inHall p

aInCell :: Amphipod -> Bool
aInCell (Amph _ p)= inCell p

inFinalCell :: Amphipod -> Bool
inFinalCell a@(Amph _ (_, c) ) = (finalColumn a) == c

finalColumn :: Amphipod -> Int
finalColumn (Amph A _) = 3
finalColumn (Amph B _) = 5
finalColumn (Amph C _) = 7
finalColumn (Amph D _) = 9

factor :: Amphipod -> Int
factor (Amph A _) = 1
factor (Amph B _) = 10
factor (Amph C _) = 100
factor (Amph D _) = 1000

colorToChar :: Color -> Char
colorToChar A = 'A'
colorToChar B = 'B'
colorToChar C = 'C'
colorToChar D = 'D'

manhattanDistance :: Point -> Point -> Int
manhattanDistance (l, r) (l', r') = (abs (l - l')) + (abs (r - r'))

shortest :: Amphipod -> Int
shortest a@(Amph _ p @(r, c))
  | inHall p = do
      let dest = finalColumn a
      (abs (c - dest)) + 1
  | otherwise = do
      let dest = finalColumn a
      (abs (c - dest)) + (abs (r - 1)) + 1

score :: Amphipod -> Point -> Int
score a@(Amph _ p0) p1 = (factor a) * (manhattanDistance p0 p1)

inRange :: Int -> (Int, Int) -> Bool
inRange x (a0, a1) = if (a0 < a1) then (a0 < x && x < a1) else (a1 < x && x < a0)

between :: Point -> (Point, Point) -> Bool
between (x, y) (p@(x0, y0), (x1, y1))
  | inHall p = (x == 2 && y == y1) || (inRange y (y0, y1) && x == 1)
  | x0 == 3 = (y == y0 && x == 2) || (inRange y (y0, y1) && x == 1)
  | otherwise = (inRange y (y0, y1) && x == 1)

validMove :: Burrow -> Amphipod -> Point -> Bool
validMove as (Amph col p0) p1@(r, c)
  |inHall p0 == inHall p1 = False
  |inCell p1 && r == 2 && (not $ any (\a@(Amph _ (_,c')) -> c == c' && inFinalCell a) as) = False
  |inCell p1 && (not $ inFinalCell (Amph col p1)) = False
  |otherwise = do
     (not.(any (\(Amph _ p) -> p == p1 || p `between` (p0, p1)))) as

complete :: Burrow -> Bool
complete = all inFinalCell

data Amphipod = Amph Color Point deriving (Eq, Ord, Show)
type Burrow = Set Amphipod

template :: [String]
template = [
  "█████████████",
  "█...........█",
  "███.█.█.█.███",
  "  █.█.█.█.█  ",
  "  █████████  "
  ]

index :: Burrow -> Map Point Color
index as = fromList $ map (\(Amph c p) -> (p, c)) $ elems as

showBurrow :: Burrow -> String
showBurrow as = do
  let ind = index as
  unlines $ map (map (\p@(r, c) -> if (p `member` ind) then colorToChar $ ind ! p else (template !! r) !! c )) [[(r, c) | c <- [0..12]] | r <- [0..4]]

data State = State Burrow (Map Point Color) Int Int deriving (Eq, Show)
instance Ord State where
  compare state1@(State _ _ _ lbs1) state2@(State _ _ _ lbs2) = compare lbs1 lbs2
    -- compare s1 s2
    -- let res = compare (length $ filter inFinalCell $ elems b2) (length $ filter inFinalCell $ elems b1)
    -- if (res == EQ) then compare s1 s2 else res

type Frame = (SortedList State, Set Burrow)

lowerBoundScore :: State -> Int
lowerBoundScore (State burrow ind soFar _) = do
  let notYetInPlace = filter (not.(cellFinished ind)) $ elems burrow
  soFar + (sum $ map (scoreByColor notYetInPlace) [A, B, C, D])
  where
    scoreByColor notYetInPlace color
      | null sameColor = 0
      | length sameColor == 1 = do
          let [only] = sameColor
          (factor only) * (shortest only)
      | otherwise = do
          let [a, b] = sameColor
          (factor a) * ((shortest a) + (shortest b) + 1)
            where sameColor = filter (\(Amph c _) -> c == color) notYetInPlace
    cellFinished ind a@(Amph col (r, c)) = (inFinalCell a) && (r == 3 || (ind !? (3, c)) == Just col)


moves'' :: State -> Amphipod -> Point -> State
moves'' (State b ind s lb ) a@(Amph c p) p' = state''
  where state' = (State b' ind' s' lb)
        state'' = (State b' ind' s' lb')
        b' = Data.Set.insert (Amph c p') $ Data.Set.delete a b
        ind' = (insert p' c $ delete p ind)
        s' = (s + (score a p'))
        lb' = lowerBoundScore state'

moves' :: (Set Burrow) -> State -> Amphipod -> IO [State]
moves' visited state@(State b _ _ _) a = do
  -- putStrLn $ show a
  return $ filter (\(State b _ _ _) -> not (b `Data.Set.member` visited)) $ map (moves'' state a) $ filter (validMove b a) (hallwaySpots ++ cellSpots)



moves :: (Set Burrow) -> State -> IO [State]
moves visited state@(State b ind s lb) = do
  -- putStrLn $ show b
  putStrLn $ showBurrow b
  potentials <- mapM (moves' visited state) $ elems b
  -- putStrLn $ show potentials
  return $ join $ potentials
  where
    cheaper :: (Map Burrow Int) -> State -> Bool
    cheaper m (State b _ s _) = s <= findWithDefault s b m

search' :: (Set Burrow) -> State -> SortedList State -> IO Frame
search' visited state@(State b ind s lb) bs
  | complete b = do
      putStrLn $ "Solution: " ++ show s
      return (toSortedList [] , visited)
  | otherwise = do
      next <- moves visited state :: IO [State]
      putStrLn $ "length: " ++ (show $ length bs)  ++ " score: " ++ (show s) ++ " lower bound: " ++ (show lb)
      -- putStrLn $ show next
      return (foldl (flip Data.SortedList.insert) bs next, Data.Set.insert b visited)

search :: Frame ->  IO Frame
search (bs, visited)
  | null bs = do return (bs, visited)
  | otherwise = do
      let (b,bs') = fromJust $ uncons bs
      search' visited b bs'


goal :: Burrow
goal = Data.Set.fromList [
  (Amph A (2, 3)), (Amph A (3, 3)),
  (Amph B (2, 5)), (Amph B (3, 5)),
  (Amph C (2, 7)), (Amph C (3, 7)),
  (Amph D (2, 9)), (Amph D (3, 9))
  ]

main :: IO ()
main = do
  ls <- lines <$> getContents
  let burrow = Data.Set.fromList $ map (\p@(r, c) -> Amph (read [(ls !! r) !! c]) p) $ join $ map (\x -> [(2, x), (3, x)]) [3,5..9]

  iterateUntilM (\(l,_) -> null l) search (singleton (State burrow (index burrow) 0 0), Data.Set.empty)
  return ()
  -- putStrLn $ show $ bests
  -- putStrLn $ show $ bests ! goal

  -- putStrLn $ showBurrow burrow
