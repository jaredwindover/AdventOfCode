module Main where

import Lib
import Data.List.Split (splitOn)
import Data.List.Index (indexed)
import Data.List.Extra (maximumOn)
import Data.Set (Set, intersection, fromList, elems, lookupLE, insert, empty, unions, union)
import Data.Map (Map, empty, insert, alter, fromList, notMember, keys, (!), assocs)
import Data.Maybe (maybe, fromJust, isJust)
import Data.Tuple.Extra (both)
import Distribution.Simple.Utils (safeHead)
import Control.Monad (mapM, join, foldM)
import Control.Monad.Loops (iterateUntilM)

readScanner :: [String] -> IO [Vec3]
readScanner input = do
  -- putStrLn $ show input
  let input' = drop 1 input
  -- putStrLn $ show input'
  let triples = map parseTriple input'
  return triples
  where
    parseTriple :: String -> Vec3
    parseTriple s = do
      let [x, y, z] = map read $ splitOn "," s
      (x, y, z)

distance :: Vec3 -> Vec3 -> Int
distance (x, y, z) (a,b,c) = ((x-a) ^ 2) + ((y-b) ^ 2) + ((z-c) ^ 2)

makeDistances :: [Vec3] -> [[Distance]]
makeDistances coords = map (\v1 -> map (\v2 -> Distance v1 v2 $ distance v1 v2) $ filter (\v2 -> v1 /= v2) coords) coords

makeRelations :: [(Int, Int, (Set Vec3,Set Vec3))] -> Map Int [Int]
makeRelations relatedPairs = foldl (\m -> \(l,r,_) -> alter (update r) l $
                                     alter (update l) r m) Data.Map.empty relatedPairs
    where
      update v ma = Just $ maybe [v] (\l -> v:l) ma

makeTraversals :: Map Int [Int] -> IO (Map Int [Int])
makeTraversals m = do
  (_, m') <- iterateUntilM filled update $ ([(0, [])], Data.Map.fromList [(0, [])])
  return m'
  where
    filled (s, m') = (length $ keys m') >= (length $ keys m)
    update ([], m') = do return ([], m')
    update (s@(k, p):ss, m') = do
      -- putStrLn $ show s
      let nks = filter (\k' -> k' `notMember` m') $ m ! k
      -- putStrLn $ show nks
      let p' = k:p
      let m'' = foldl (\mt -> \kt -> Data.Map.insert kt p' mt) m' nks
      -- putStrLn $ show m'
      return (ss ++ (map (\k' -> (k', p')) nks), m'')

type Mat3 = (Vec3,
             Vec3,
             Vec3)

type Vec3 = (Int, Int, Int)

rotx :: Mat3
rotx =((1, 0, 0),
       (0, 0, -1),
       (0, 1, 0))
roty :: Mat3
roty =((0, 0, -1),
       (0, 1, 0),
       (1, 0, 0))
rotz :: Mat3
rotz =((0, -1, 0),
       (1, 0, 0),
       (0, 0, 1))

raiseMatrix3 :: Mat3 -> Int -> Mat3
raiseMatrix3 _ 0 = ((1, 0, 0),
                   (0, 1, 0),
                   (0, 0, 1))
raiseMatrix3 x n = multMatrices3 x (raiseMatrix3 x (n - 1))

row3 :: Mat3 -> Int -> Vec3
row3 (v0,v1,v2) 0 = v0
row3 (v0,v1,v2) 1 = v1
row3 (v0,v1,v2) 2 = v2

col3 :: Mat3 -> Int -> Vec3
col3 ((a, b, c),
      (d, e, f),
      (g, h, i)
     ) 0 = (a, d, g)
col3 ((a, b, c),
      (d, e, f),
      (g, h, i)
     ) 1 = (b, e, h)
col3 ((a, b, c),
      (d, e, f),
      (g, h, i)
     ) 2 = (c, f, i)

sum3 :: Vec3 -> Vec3 -> Vec3
sum3 (a, b, c) (a', b', c') = (a + a', b + b', c + c')

diff3 :: Vec3 -> Vec3 -> Vec3
diff3 (a, b, c) (a', b', c') = (a - a', b - b', c - c')

div3 :: Vec3 -> Int -> Vec3
div3 (a, b, c) x = (a `div` x, b `div` x, c `div` x)

multMatrices3 :: Mat3 -> Mat3 -> Mat3
multMatrices3 m0 m1 = ((f 0 0, f 0 1, f 0 2),
                       (f 1 0, f 1 1, f 1 2),
                       (f 2 0, f 2 1, f 2 2))
  where f x y = (row3 m0 x) `dot` (col3 m1 y)

fullRots :: Set Mat3
fullRots = Data.Set.fromList [ multMatrices3 (raiseMatrix3 rotx a) (multMatrices3 (raiseMatrix3 roty b) (raiseMatrix3 rotz c))| a <- [0..3], b <- [0..3], c <- [0..3]]

dot :: Vec3 -> Vec3 -> Int
dot (a, b, c) (a', b', c') = a*a' + b*b' + c*c'

rotate :: Mat3 -> Vec3 -> Vec3
rotate m v = (f 0, f 1, f 2)
  where
    f x = (col3 m x) `dot` v

-- inCoordinatesOf :: [Vec3] -> [Vec3] -> [Vec3]
-- vs `inCoordinatesOf` vsbase = do
--   let potentials = map (\rmat -> map (rotate rmat) vs) fullRots
--   let diffs = map (\vs' -> (vs', map (\v' -> vsbase `diff3` v') vs')) potentials
--   maximumOn (\(vs', ds) -> maximum $ elems $ count ds) diffs

count :: (Ord k) => [k] -> Map k Int
count xs = foldl (\m -> \k -> alter (\mb -> Just $ maybe 1 (+1) mb) k m) Data.Map.empty xs

first :: (a -> Bool) -> [a] -> Maybe a
first predicate = safeHead.(dropWhile $ not.predicate)

data Distance = Distance Vec3 Vec3 Int deriving (Show)
instance Ord Distance where compare (Distance _ _ d1) (Distance _ _ d2) = compare d1 d2
instance Eq Distance where (Distance _ _ d1) == (Distance _ _ d2) = d1 == d2

convert :: (Int, Int, Set Distance, Set Distance) -> IO (Int, Int, (Set Vec3, Set Vec3))
convert (l,r, les, res) = do
  let les' = Data.Set.fromList $ join $ fmap (\(Distance v1 v2 _) -> [v1, v2]) $ elems les
  let res' = Data.Set.fromList $ join $ fmap (\(Distance v1 v2 _) -> [v1, v2]) $ elems res
  return (l,r, (les', res'))

overlap :: [Distance] -> [Distance] -> (Set Distance, Set Distance)
overlap ds1 ds2 = ((Data.Set.fromList ds1) `intersection` (Data.Set.fromList ds2),
                   (Data.Set.fromList ds2) `intersection` (Data.Set.fromList ds1))

found :: [Distance] -> [[Distance]] -> Maybe (Set Distance, Set Distance)
found d ds = first (\(dsl',_) -> (length dsl') >= 11) $ map (\d2 -> (overlap d d2)) ds

enough :: [[Distance]] -> [[Distance]] -> Maybe (Set Distance, Set Distance)
enough ds1 ds2 = do
  let findings = foldl update [] ds2
  if ((length $ findings) >= 11) then (Just $ both unions $ unzip findings) else Nothing
  where
    update :: [(Set Distance, Set Distance)] -> [Distance] -> [(Set Distance, Set Distance)]
    update acc d2 = maybe acc (:acc) $ found d2 ds1

com :: [Vec3] -> Vec3
com vs = foldl1 sum3 vs

affine :: [Vec3] -> [Vec3] -> Vec3
affine v1s v2s = (diff3 (com v2s) (com v1s)) `div3` 12

affineCompare :: [Vec3] -> [Vec3] -> IO (Maybe Vec3)
affineCompare v1s v2s = do
  -- putStrLn $ show v1s
  -- putStrLn $ show v2s
  let a = affine v1s v2s
  putStrLn $ show a
  let v2s' = map (sum3 a) v1s
  -- putStrLn $ show $ Data.Set.fromList v1s'
  -- putStrLn $ show $ Data.Set.fromList v1s
  if (Data.Set.fromList v2s) == (Data.Set.fromList v2s') then return $ Just a else return Nothing

makeMap :: [(Int, Int, (Set Vec3, Set Vec3))] -> Map (Int, Int) (Set Vec3, Set Vec3)
makeMap xs = foldl (\m -> \(a,b, (l, r)) -> Data.Map.insert (b,a) (r, l)
                     $ Data.Map.insert (a,b) (l, r) m) Data.Map.empty xs

accumulate' :: (Set Vec3, Set Vec3) -> [Vec3] -> IO [Vec3]
accumulate' (commonOther, commonThis) vs = do
  mmbs <- mapM find $ elems fullRots
  -- putStrLn $ show mmbs
  let (m, a) = fromJust $ fromJust $ first isJust mmbs
  putStrLn $ "(Matrix, Affine): " ++ show (m, a)
  let newVecs = map (\v -> sum3 a (rotate m v)) vs
  putStrLn $ show $ length newVecs
  putStrLn $ "This: " ++ show commonThis
  putStrLn $ "Other: " ++ show commonOther
  putStrLn $ show $ length $ intersection (Data.Set.fromList vs) commonThis
  putStrLn $ show $ length $ intersection (Data.Set.fromList newVecs) commonOther
  putStrLn $ show $ length $ intersection (Data.Set.fromList vs) commonOther
  putStrLn $ show $ length $ intersection (Data.Set.fromList newVecs) commonThis
  putStrLn $ show vs
  return newVecs
  where
    find :: Mat3 -> IO (Maybe (Mat3, Vec3))
    find rotmat = do
      r <- affineCompare (map (rotate rotmat) $ elems commonThis) $ elems commonOther
      return $ fmap (\x -> (rotmat, x)) r


accumulate :: Map (Int, Int) (Set Vec3, Set Vec3) -> [[Vec3]] -> Set Vec3 -> (Int, [Int]) -> IO (Set Vec3)
accumulate commonMap results sofar (k, p) = do
  putStrLn $ "Starting for" ++ show (k, p)
  (_, newVecs) <- foldM folder (k, (results !! k)) p
  return $ union (Data.Set.fromList newVecs) sofar
  where folder (k', vSoFar) p' = do
          r <- accumulate' (commonMap ! (k', p')) vSoFar
          return $ (p', r)

main :: IO ()
main = do
  ls <- splitOn [""] <$> lines <$> getContents :: IO [[String]]
  results <- mapM readScanner ls
  let ds = map makeDistances results :: [[[Distance]]]
  let ixs = [(li, ri, enough l r) | (li, l) <- indexed ds, (ri, r) <- indexed ds, li < ri]
  -- putStrLn $ show ixs
  let matchings = map (\(a,b, (l, r)) -> (a,b,l,r)) $ map (\(a,b,c) -> (a,b, fromJust c)) $ filter (\(_,_,b) -> isJust b) ixs
  -- putStrLn $ show matchings

  converted <- mapM convert matchings
  -- mapM_ (\(li,ri,(sl, sr)) -> do putStrLn $ show (li,ri,length sl, length sr)) converted

  -- let (_, _, (probleml, problemr)) = converted !! 14
  -- mapM_ (\(_,_,dl,dr,_) -> do
  --           putStrLn $ show $ count $ join $ fmap (\(Distance v1 v2 d) -> [v1, v2]) $ elems dl
  --           putStrLn $ show $ count $ join $ fmap (\(Distance v1 v2 d) -> [v1, v2]) $ elems dr) [matchings !! 14]
  -- putStrLn "Hello"
  -- putStrLn $ show $ probleml
  -- putStrLn $ show $  problemr

  let setMap = makeMap converted

  let relations = makeRelations converted
  traversals <- makeTraversals relations

  scanners <- foldM (accumulate setMap results) Data.Set.empty $ assocs traversals
  -- in0Coords <- foldM (accumulate setMap results) Data.Set.empty $ assocs traversals
  putStrLn $ show converted
  -- putStrLn $ show setMap
  -- putStrLn $ show in0Coords
  putStrLn $ show $ length in0Coords

  -- putStrLn $ show relations
  -- putStrLn $ show traversals
  -- putStrLn $ show $ length fullRots
  -- putStrLn $ show $ results !! 0
  -- putStrLn $ show $ ds !! 0
