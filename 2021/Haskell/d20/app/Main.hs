module Main where

import Lib
import Data.Bits (shift)
import Data.List.Index (indexed)
import Data.Map (Map, empty, insert, keys, findWithDefault, assocs, alter, elems, (!))
import Data.Set (Set, fromList)
import Data.Tuple.Extra (both)
import qualified Data.Set (empty, insert)
import Prelude hiding (lookup)

data Sparse = Sparse (Map Position Char) Char deriving (Eq)
type Position = (Int, Int)
type Algorithm = String

instance Show Sparse where
  show s@(Sparse m d) = do
    let ((rmin, rmax), (cmin, cmax)) = both (\l -> (minimum l, maximum l)) $ unzip $ keys m
    unlines $ map (\r -> map (\c -> lookup s (r, c)) [(cmin - 1)..(cmax + 1)]) [(rmin - 1)..(rmax + 1)]


add :: Position -> Position -> Position
add (a, b) (a', b') = (a + a', b + b')

lookup :: Sparse -> Position -> Char
lookup (Sparse m d) p = findWithDefault d p m

around :: [Position]
around = [
  (-1, -1),
  (-1, 0),
  (-1, 1),
  (0, -1),
  -- (0, 0),
  (0, 1),
  (1, -1),
  (1, 0),
  (1, 1)
  ]

aroundIncluding :: [Position]
aroundIncluding = [
  (-1, -1),
  (-1, 0),
  (-1, 1),
  (0, -1),
  (0, 0),
  (0, 1),
  (1, -1),
  (1, 0),
  (1, 1)
  ]

allAround :: Position -> [Position]
allAround p = map (add p) around

allAroundIncluding :: Position -> [Position]
allAroundIncluding p = map (add p) aroundIncluding

candidates :: Sparse -> Set Position
candidates (Sparse m _) = foldl (\s -> \k -> foldl (flip Data.Set.insert) s $ allAround k) Data.Set.empty (keys m)

bin2dec :: [Int] -> Int
bin2dec = foldl (\acc -> \v -> (shift acc 1) + v) 0

getIndex :: Sparse -> Position -> Int
getIndex s p = bin2dec $ map decode $ map (lookup s) $ allAroundIncluding p
  where decode '#' = 1
        decode _ = 0

getUpdate :: Algorithm -> Sparse -> Position -> Char
getUpdate a s p = a !! (getIndex s p)

handleDefault :: Algorithm -> Char -> Char
handleDefault a '.' = a !! 0
handleDefault a _ = a !! ((length a) - 1)

update :: Algorithm -> Sparse -> Sparse
update a s@(Sparse m d) = do
  let cs = candidates s
  let newSparseMat = foldl (\m -> \p -> insert p (getUpdate a s p) m) empty $ cs
  let newDefault = handleDefault a d
  Sparse newSparseMat newDefault

count :: (Ord k) => [k] -> Map k Int
count xs = foldl (\m -> \k -> alter (\mb -> Just $ maybe 1 (+1) mb) k m) Data.Map.empty xs



main :: IO ()
main = do
  ls <- lines <$> getContents
  let algorithm = ls !! 0
  let input = drop 2 ls
  let sparseMap = foldl (\m -> \(ri, r) -> foldl (\m' -> \(ci, c) -> insert (ri, ci) c m') m $ indexed r) empty $ indexed input
  let sparse = Sparse sparseMap '.'
  -- putStrLn $ show sparse
  -- putStrLn $ show $ update algorithm sparse
  -- putStrLn $ show $ update algorithm $ update algorithm sparse
  -- let (Sparse mat2Rnds d) = update algorithm $ update algorithm sparse
  -- putStrLn $ show $ length input
  -- putStrLn $ show $ length (input !! 0)
  -- putStrLn $ show ((rmin, rmax), (cmin, cmax))
  let s@(Sparse mat2Rnds d) = head $ drop 50 $ iterate (update algorithm) sparse
  putStrLn $ show s
  putStrLn $ show $ (count $ elems mat2Rnds) ! '#'
