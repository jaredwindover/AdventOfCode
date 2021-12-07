{-# LANGUAGE NamedFieldPuns #-}

import Data.Char;
import Data.Bits;
import Data.Maybe;

data Binary = Zero | One deriving (Eq, Show)

bin2dec :: [Int] -> Int
bin2dec = foldl (\acc -> \v -> (shift acc 1) + v) 0

data Entry  = Entry {
  count :: Int,
  child :: Maybe Node
  } deriving (Eq, Show)

data Node = Node{
  entry0 :: Entry,
  entry1 :: Entry
  } deriving (Eq, Show)

newEntry :: Entry
newEntry = Entry {count = 0, child = Nothing}

newNode :: Node
newNode = Node {entry0 = newEntry, entry1 = newEntry}

updateNode :: Node -> [Binary] -> Node
updateNode node [] = node
updateNode (Node {entry0, entry1}) (x:xs)
  | x == Zero = Node {entry0 = updateEntry entry0 xs, entry1}
  | otherwise = Node {entry0, entry1 = updateEntry entry1 xs}
  where
    updateEntry (Entry {count, child}) rest = Entry {count = count + 1, child = updateMaybe child rest}
    updateMaybe x [] = x
    updateMaybe m rest = Just $ updateNode (fromMaybe newNode m) rest

findO2 :: Node -> [Int]
findO2 (Node {entry0 = Entry{child = Nothing},
              entry1 = Entry{child = Nothing}}) = []
findO2 (Node {entry0 = Entry{child = Just child0},
              entry1 = Entry{child = Nothing}}) = 0:findO2(child0)
findO2 (Node {entry0 = Entry{child = Nothing},
              entry1 = Entry{child = Just child1}}) = 1:findO2(child1)
findO2 (Node {entry0 = Entry{count = count0, child = Just child0},
              entry1 = Entry{count = count1, child = Just child1}})
  | count0 > count1 = 0:findO2(child0)
  | otherwise = 1:findO2(child1)

findCO2 :: Node -> [Int]
findCO2 (Node {entry0 = Entry{child = Nothing},
              entry1 = Entry{child = Nothing}}) = []
findCO2 (Node {entry0 = Entry{child = Just child0},
              entry1 = Entry{child = Nothing}}) = 0:findCO2(child0)
findCO2 (Node {entry0 = Entry{child = Nothing},
              entry1 = Entry{child = Just child1}}) = 1:findCO2(child1)
findCO2 (Node {entry0 = Entry{count = count0, child = Just child0},
              entry1 = Entry{count = count1, child = Just child1}})
  | count1 < count0 = 1:findCO2(child1)
  | otherwise = 0:findCO2(child0)


main = do
  -- nums <- map (map digitToInt) <$> lines <$> getContents
  let nums = [[0, 1, 0], [1, 0, 1]]
  let bins = map (map convert) nums
  let tree = foldl (updateNode) newNode bins
  putStrLn $ show tree
  let o2 = findO2 tree
  let co2 = findCO2 tree
  putStrLn $ show o2
  putStrLn $ show $ nums !! 0
  -- let o2 = bin2dec $ findO2 tree
  -- let co2 = bin2dec $ findCO2 tree
  -- putStrLn $ show (o2*co2)

  -- let gammaBin = map coerce $ foldr1 (zipWith (+)) nums
  -- let gamma = bin2dec gammaBin
  -- let epsilon = bin2dec $ map (\x -> 1 - x) gammaBin
  -- putStrLn $ show $ gamma * epsilon
    where
      coerce x
        | x < 0 = 0
        | otherwise = 1
      convert x
        | x == 0 = Zero
        | otherwise = One
