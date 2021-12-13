import Data.Char

triple :: [a] -> [(a,a,a)]
triple xs = map (\(a, (b, c)) -> (a, b, c)) $ zip xs $ zip (tail xs) (tail $ tail xs)

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

main = do
  intByLine <- readInput
  let windowed = map (\(a, b, c) -> a + b + c) $ triple intByLine
  let increases = countIncreases windowed
  putStrLn $ show increases
