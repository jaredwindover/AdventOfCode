module Main where

import Lib
import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Tuple.Extra (second)
import Data.Set (Set, union, unions, difference, empty, fromList, elems)
import Data.Maybe (catMaybes)
import Control.Monad (join)

data Cuboid = Cuboid Bool Range Range Range deriving (Eq, Ord, Show)
type Point = (Int, Int, Int)
type Range = (Int, Int)

makeCuboid :: (Bool, Range, Range, Range) -> Cuboid
makeCuboid (b, x, y, z) = Cuboid b x y z

getPoints :: Cuboid -> Set Point
getPoints (Cuboid _ (minx,maxx) (miny,maxy) (minz,maxz)) =
  fromList [(x,y,z) | x <- [minx..maxx], y <- [miny..maxy], z <- [minz..maxz]]

readOn :: String -> Bool
readOn "on" = True
readOn "off" = False

update :: Set Point -> Cuboid -> Set Point
update points c@(Cuboid False _ _ _) = difference points $ getPoints c
update points c@(Cuboid True _ _ _) = union points $ getPoints c

-- updateFast :: Set Cuboid -> Cuboid -> Set Cuboid
-- updateFast cuboids c@(Cuboid False _ _ _) = differenceCuboids cuboids c
-- updateFast cuboids c@(Cuboid True _ _ _) = unionCuboids cuboids c

overlapr :: Range -> Range -> Bool
overlapr (l, r) (l', r') = r' >= l && r >= l'

overlapc :: Cuboid -> Cuboid -> Bool
overlapc (Cuboid _ x y z) (Cuboid _ x' y' z') = (overlapr x x') && (overlapr y y') && (overlapr z z')

applyCuboids :: [Cuboid] -> Set Point
applyCuboids cuboids = foldl update empty cuboids

-- applyCuboidsFast :: [Cuboid] -> Set Cuboid
-- applyCuboidsFast cuboids = foldl updateFast empty cuboids

inCuboid :: Cuboid -> Point -> Bool
inCuboid (Cuboid _ xr yr zr) (x, y, z) = (inRange xr x) && (inRange yr y) && (inRange zr z)

inRange :: Range -> Int -> Bool
inRange (l, r) x = l <= x && x <= r

regionOfConsideration :: Cuboid
regionOfConsideration = Cuboid True (-50,50) (-50,50) (-50,50)

sizer :: Range -> Int
sizer (l, r) = r + 1 - l

sizec :: Cuboid -> Int
sizec (Cuboid _ x y z) = (sizer x) * (sizer y) * (sizer z)

getRanges :: [Int] -> Range -> [Range]
getRanges vs (l, r) = do
  let overlaps = sort [p | p <- vs, l <= p, p <= r]
  let naiveResult = zip overlaps (drop 1 overlaps)
  let first = head naiveResult
  first:(map (\(l,r) -> (l + 1, r)) $ drop 1 naiveResult)

updateFast :: ([Int], [Int], [Int]) -> Set Cuboid -> Cuboid -> Set Cuboid
updateFast (xs, ys, zs) cs (Cuboid b x y z) = do
  let xRanges = getRanges xs x
  let yRanges = getRanges ys y
  let zRanges = getRanges zs z
  let cs' = fromList [Cuboid True x' y' z' | x' <- xRanges, y' <- yRanges, z' <- zRanges]
  if (b) then union cs cs' else difference cs cs'

-- addPoints :: (Set Int, Set Int, Set Int) -> Cuboid -> (Set Int, Set Int, Set Int)
-- addPoints (xs, ys, zs) (Cuboid _ (x, x') (y, y') (z, z')) = do
--   let xs' = insert x $ insert x' xs
--   let ys' = insert y $ insert y' ys
--   let zs' = insert z $ insert z' zs
--   (xs', ys', zs')

data DisjointCuboids = DisjointCuboids [Cuboid] deriving (Eq, Show)

rangeDiff :: Range -> Range -> ([Range], Maybe Range)
rangeDiff rng0@(l0, r0) rng1@(l1, r1)
  | not $ overlapr rng0 rng1 = ([rng0], Nothing)
  | otherwise = do
      let leftRange = if (l0 < l1) then Just (l0, l1 - 1) else Nothing
      let rightRange = if (r1 < r0) then Just (r1 + 1, r0) else Nothing
      (catMaybes [leftRange, rightRange],
       Just (if (l0 < l1) then l1 else l0, if (r1 < r0) then r1 else r0))

data Dimension = X | Y | Z

restrict :: Dimension -> Cuboid -> [Range] -> DisjointCuboids
restrict dim (Cuboid b x y z) rs = DisjointCuboids $ map (mapper dim) rs
  where
    mapper X = (\r -> Cuboid b r y z)
    mapper Y = (\r -> Cuboid b x r z)
    mapper Z = (\r -> Cuboid b x y r)

cuboidDiff'' :: Cuboid -> Cuboid -> DisjointCuboids
cuboidDiff'' c1@(Cuboid _ x1 y1 z1) c2@(Cuboid _ x2 y2 z2) = do
  let (x', Just dx) = rangeDiff x1 x2
  let (y', Just dy) = rangeDiff y1 y2
  let (z', Just dz) = rangeDiff z1 z2
  let (DisjointCuboids xRanges) = restrict X c1 x'
  let (DisjointCuboids [c1']) = restrict X c1 [dx]
  let (DisjointCuboids yRanges) = restrict Y c1' y'
  let (DisjointCuboids [c1'']) = restrict Y c1' [dy]
  let (DisjointCuboids zRanges) = restrict Z c1'' z'
  DisjointCuboids $ xRanges ++ yRanges ++ zRanges


cuboidDiff' :: Cuboid -> DisjointCuboids -> DisjointCuboids
cuboidDiff' c (DisjointCuboids []) = DisjointCuboids [c]
cuboidDiff' c (DisjointCuboids (c':cs)) = do
  let (DisjointCuboids dcs) = cuboidDiff'' c c'
  DisjointCuboids $ join $ map (\(DisjointCuboids dcs') -> dcs') $ map (\cn -> cuboidDiff cn $ DisjointCuboids cs) dcs

cuboidDiff :: Cuboid -> DisjointCuboids -> DisjointCuboids
cuboidDiff c (DisjointCuboids dcs) = do
  let overlapping = DisjointCuboids $ filter (overlapc c) dcs
  cuboidDiff' c overlapping

insert' :: DisjointCuboids -> DisjointCuboids -> DisjointCuboids
insert' (DisjointCuboids []) dcs = dcs
insert' dcs (DisjointCuboids []) = dcs
insert' (DisjointCuboids (lc:lrest)) dcs@(DisjointCuboids rcs) = do
  let (DisjointCuboids ldiffcs) = cuboidDiff lc dcs
  DisjointCuboids $ ldiffcs ++ rcs


insert :: Cuboid -> DisjointCuboids -> DisjointCuboids
insert c = insert' (DisjointCuboids [c])

main :: IO ()
main = do
  cs <- map makeCuboid <$>
    map (\(b, [[x0,x1], [y0,y1], [z0,z1]]) -> (b, (x0, x1), (y0, y1), (z0, z1))) <$>
    map (second $ (map $ (map read).(splitOn "..").(drop 2)).(splitOn ",")) <$>
    map (\[b, r] -> (readOn b, r)) <$>
    map words  <$>
    lines <$>
    getContents
  let (DisjointCuboids disjoint) = foldl (\dcs -> \c -> insert c dcs) (DisjointCuboids []) $ reverse cs
  putStrLn $ show $ disjoint !! 0
  putStrLn $ show $ length disjoint
  putStrLn $ show $ sum $ map sizec $ filter (\(Cuboid b _ _ _) -> b) disjoint
  -- let (xs, ys, zs) = foldl addPoints (empty, empty, empty) cs
  -- let ranges = (sort $ elems xs, sort $ elems ys, sort $ elems zs)
  -- putStrLn $ show $ length $ elems $ applyCuboids cs
