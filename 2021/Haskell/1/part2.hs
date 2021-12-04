import Data.Char

readInput :: IO [Int]
readInput = do
  byLine <- lines <$> getContents
  return $ map read byLine

data Accumulator = Accumulator { prev :: Maybe Int,
                                 count :: Int
                               } deriving Show

countIncreases :: [Int] -> Int
countIncreases = count.(foldl accumulate Accumulator {prev = Nothing, count = 0})
  where
    accumulate Accumulator{prev=Nothing, count=count} value = Accumulator{prev=Just value, count=count }
    accumulate Accumulator{prev = Just prev, count=count} value
      | value > prev = Accumulator{prev = Just value, count=count + 1}
      | otherwise = Accumulator{prev = Just value, count=count}


threeMeasureSlidingWindowRecursive :: [Int] -> [Int]
threeMeasureSlidingWindowRecursive [] = []
threeMeasureSlidingWindowRecursive [x] = []
threeMeasureSlidingWindowRecursive [x, y] = []
threeMeasureSlidingWindowRecursive (x:y:z:zs) = x + y + z:threeMeasureSlidingWindow (y:z:zs)



threeMeasureSlidingWindow :: [Int] -> [Int]
threeMeasureSlidingWindow = (drop 2).sel1.(foldl accumulate ([], 0, 0, 0))
  where
    accumulate (result, a, b, c) value = (result ++ [a], b + value, c + value, value)
    sel1 (result, _, _, _) = result

main = do
  intByLine <- readInput
  let windowed = threeMeasureSlidingWindow intByLine
  let increases = countIncreases windowed
  putStrLn $ show increases
