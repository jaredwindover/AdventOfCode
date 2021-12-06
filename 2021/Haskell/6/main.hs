import Data.List.Split (splitOn)
import Data.Map hiding (map, foldl, take, drop)
import Data.Maybe

getCounts = foldl putInMap empty
  where putInMap = flip $ alter (\x -> Just (maybe 1 (+ 1) x))

updateMap m = do
  let m2 = mapKeys (subtract 1) m
  let count0 = findWithDefault 0 (-1) m2
  let m3 = insert 8 count0 m2
  let m4 = alter (\x -> Just (maybe count0 (+count0) x)) 6 m3
  delete (-1) m4

main = do
  input <- map read <$> splitOn "," <$> getContents
  putStrLn $ show $ sum $ elems $ head $ drop 256 $ iterate updateMap $ getCounts input
