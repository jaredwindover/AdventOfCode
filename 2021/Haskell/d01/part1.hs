import Data.Char

readInput :: IO [Int]
readInput = do
  byLine <- lines <$> getContents
  return $ map read byLine

countIncreasesRecursive :: [Int] -> Int
countIncreasesRecursive [] = 0
countIncreasesRecursive [x] = 0
countIncreasesRecursive (x0:x1:xs)
  | x0 < x1 = 1 + countIncreases (x1:xs)
  | otherwise = countIncreases (x1:xs)


data Accumulator = Accumulator { prev :: Maybe Int,
                                 count :: Int
                               } deriving Show

accumulate :: Accumulator -> Int -> Accumulator
accumulate Accumulator{prev=Nothing, count=count} value = Accumulator{prev=Just value, count=count }
accumulate Accumulator{prev = Just prev, count=count} value
  | value > prev = Accumulator{prev = Just value, count=count + 1}
  | otherwise = Accumulator{prev = Just value, count=count}

countIncreases :: [Int] -> Int
countIncreases = count.(foldl accumulate Accumulator {prev = Nothing, count = 0})


main = do
  intByLine <- readInput
  let increases = countIncreases intByLine
  putStrLn $ show increases
