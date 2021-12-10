import Prelude hiding (null, drop)
import Control.Monad
import Control.Monad.ListM (scanM)
import Data.List (sort)
import Data.Set (Set, empty, member, union, fromList)
import Data.Sequence (Seq, singleton, null, index, (><), fromList, drop)

data Accumulator = Accumulator Int (Set (Int, Int)) (Seq (Int, Int))

queueNotEmpty :: Accumulator -> Bool
queueNotEmpty (Accumulator size visited queue) = not $ null queue

main = do
  matrix <- map (map (\c -> read [c])) <$> lines <$> getContents :: IO [[Int]]
  let height = length matrix
  let width = length $ matrix !! 0
  putStrLn $ show height
  putStrLn $ show width
  let cells = join $ map (\h -> map (\w -> (h, w)) [0..width - 1]) [0..height - 1]
  let basinCenters = filter (evaluate matrix height width) cells :: [(Int, Int)]
  let basinSizes = map (basinSize height width matrix) basinCenters :: [Int]
  let b0:b1:b2:rest = reverse $ sort basinSizes
  putStrLn $ show $ sort basinSizes
  putStrLn $ show $ b0 * b1 * b2
    where
      inside (h, w) height width matrix = not $ h < 0 || w < 0 || h >= height || w >= width
      valueAt matrix (h, w) = (matrix !! h) !! w
      evaluate matrix height width p = (==0).length $ filter (\c -> inside c height width matrix && (valueAt matrix c) <= (valueAt matrix p)) $ compareWith p
      compareWith (h, w) = map (\(dh, dw) -> (h + dh, w + dw)) comparisons
      comparisons = [(0, 1), (0, -1), (1, 0), (-1, 0)]
      basinSize height width matrix basinCenter = head
        $ map (\(Accumulator size _ _) -> size)
        $ dropWhile queueNotEmpty
        $ scanl (update height width matrix) (Accumulator 0 (Data.Set.fromList [basinCenter]) $ singleton basinCenter)  [0..]
      update height width matrix (Accumulator size enqueued queue) _
        | inBasin = do
            let newPoints = filter (\c -> (inside c height width matrix) && (not $ member c enqueued)) $ compareWith p
            let newQueue = rest >< Data.Sequence.fromList newPoints
            let newEnqueued = union enqueued $ Data.Set.fromList newPoints
            Accumulator (size + 1) newEnqueued newQueue
        | otherwise = do
            Accumulator size enqueued rest
        where
          p = index queue 0
          rest = drop 1 queue
          inBasin = (valueAt matrix p) < 9
