import Data.List.Split (splitOn)
import Data.Map (
  Map,
  empty,
  member,
  adjust,
  insert,
  mapKeys,
  alter,
  delete,
  findWithDefault,
  toList,
  (!))

main :: IO ()
main = do
  input <- map read <$> splitOn "," <$> getContents
  let withCounts = foldl putInMap empty input
  putStrLn $ show $ take 5 $ iterate updateMap withCounts
  let mapAt80 =  head $ drop 256 $ iterate updateMap withCounts
  putStrLn $ show $ sum $ map snd $ toList mapAt80
    where
      putInMap :: Map Int Int -> Int -> Map Int Int
      putInMap mapSoFar newValue
        | member newValue mapSoFar = adjust (+ 1) newValue mapSoFar
        | otherwise = insert newValue 1 mapSoFar
      updateMap :: Map Int Int -> Map Int Int
      updateMap m = do
        let m2 = mapKeys (subtract 1) m
        let c0 = findWithDefault 0 (-1) m2
        let m3 = insert 8 c0 m2
        let m4 = alter (\x -> Just $ maybe c0 (+ c0) x) 6 m3
        let m5 = delete (-1) m4
        m5
