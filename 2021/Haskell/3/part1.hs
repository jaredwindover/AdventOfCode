import Data.Char;
import Data.Bits;

bin2dec :: [Int] -> Int
bin2dec = foldl (\acc -> \v -> (shift acc 1) + v) 0

main = do
  nums <- map (map ((\x -> (x*2) - 1).digitToInt)) <$> lines <$> getContents
  let gammaBin = map coerce $ foldr1 (zipWith (+)) nums
  let gamma = bin2dec gammaBin
  let epsilon = bin2dec $ map (\x -> 1 - x) gammaBin
  putStrLn $ show $ gamma * epsilon
    where
      coerce x
        | x < 0 = 0
        | otherwise = 1
