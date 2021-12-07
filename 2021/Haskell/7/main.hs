import Data.List.Split
import Data.List

compare' :: Int -> Int -> Int
compare' x y = (n * (n + 1)) `div` 2
  where n = abs (x - y)

main = do
  nums <- sort <$> map read <$> splitOn "," <$> getContents :: IO [Int]
  let minNum = minimum nums
  let maxNum = maximum nums
  let answer = minimum (map (evaluate nums) [minNum..maxNum])
  putStrLn $ show answer
    where evaluate nums value = sum $ map (compare' value) nums
