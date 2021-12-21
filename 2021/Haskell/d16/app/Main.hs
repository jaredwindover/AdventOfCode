module Main where

import Prelude hiding (lookup)
import Data.Char (digitToInt);
import Data.Bits (shift);
import Data.Map (Map, alter, empty, (!?), fromList)
import Data.Maybe (maybe, catMaybes, fromMaybe)
import Data.List (splitAt)
import Control.Monad
import Lib

type Matrix = [[Int]]
type Cell = (Int, Int)

allAround :: [(Int, Int)]
allAround = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (-1, 1), (1, -1), (-1,-1)]

fourAround :: [(Int, Int)]
fourAround = [(0, 1), (0, -1), (1, 0), (-1, 0)]

aroundCellInMatrix :: [(Int, Int)] -> Int -> Int -> (Int, Int) -> [(Int, Int)]
aroundCellInMatrix ds xmax ymax (px, py) = filter inside ns
  where
    ns = map (\(dx, dy) -> (px + dx, py + dy)) ds
    inside = (\(nx, ny) -> (0 <= nx) && (0 <= ny) && (nx < xmax) && (ny < ymax))

nextFour :: Int -> Int -> (Int, Int) -> [(Int, Int)]
nextFour = aroundCellInMatrix fourAround

count :: (Ord k) => [k] -> Map k Int
count xs = foldl (\m -> \k -> alter (\mb -> Just $ maybe 1 (+1) mb) k m) empty xs

first :: (a -> Bool) -> [a] -> a
first predicate = head.(dropWhile $ not.predicate)

charToString :: Char -> String
charToString x = [x]

bin2char :: Int -> Char
bin2char 0 = '0'
bin2char 1 = '1'
bin2char _ = '?'

bin2dec :: [Int] -> Int
bin2dec = foldl (\acc -> \v -> (shift acc 1) + v) 0

readMatrix :: IO Matrix
readMatrix = do
  map (map (read.charToString)) <$> lines <$> getContents

charToBin :: Char -> [Int]
charToBin '0' = [0,0,0,0]
charToBin '1' = [0,0,0,1]
charToBin '2' = [0,0,1,0]
charToBin '3' = [0,0,1,1]
charToBin '4' = [0,1,0,0]
charToBin '5' = [0,1,0,1]
charToBin '6' = [0,1,1,0]
charToBin '7' = [0,1,1,1]
charToBin '8' = [1,0,0,0]
charToBin '9' = [1,0,0,1]
charToBin 'A' = [1,0,1,0]
charToBin 'B' = [1,0,1,1]
charToBin 'C' = [1,1,0,0]
charToBin 'D' = [1,1,0,1]
charToBin 'E' = [1,1,1,0]
charToBin 'F' = [1,1,1,1]
charToBin _ = [-1]

type HandlePacket = Context -> Packet -> Context
data Type = Type Int
data Version = Version Int
data Packet = Literal Version Int | Operator Version Type [Packet]
data Length = Bits Int | Packets Int
data Frame = Start |
             ParseLiteral Version Type [Int] HandlePacket|
             ParseOperator Version Type (Maybe Length) [Packet] HandlePacket
data Context = [Packet]

version :: Packet -> [Version]
version (Literal v _) = [v]
version (Operator v _ subs) = v: join $ map version subs

update :: ([Frame], [Int], Context) -> ([Frame], [Int], Context)

update (_, [], c) = ([], [], c)
update ([], rest, c) = ([Start], rest, c)

update (Start:xs, (a:b:c:d:e:f:rs), c) = do
  let version = Version $ bin2dec [a,b,c]
  let t = Type $ bin2dec [d,e,f]
  let newFrame = if (t == Type 4)
        then ParseLiteral verion t [] (c -> p -> c ++ [p])
        else ParseOperator version t Nothing [] (c -> p -> c ++ [p])
  (newFrame:xs, rs, c)

update (Start:xs, _, c) = (xs, [], c)

update ((ParseLiteral v t bs hpf):xs, rest, c) = do
  let (i:a:b:c:d:rs) = rest
  let bs' = bs ++ [a,b,c,d]
  if (i == 0) then finishLiteral $ bin2dec bs' else ((ParseLiteral v t bs' hpf):xs, rs, c)
    where
      finishLiteral val = do
        let packet = Literal v val
        let c' = hpf c packet
        (xs, rs, c')

update ((ParseOperator v t Nothing _ hpf):xs, rest, c) = do
  let (l, rs) = getLength rest
  ((ParseOperator v t (Just l) [] hpf):xs, rest, c)

update ((ParseOperator v t (Just (Bits l)) _ hpf):xs, rest, c) = do
  let (ps, rs) = splitAt l rest
  let packets = parse ps
  let packet = Operator v t packets
  let c'= hpf c packet
  (xs, rs, c')

update ((ParseOperator v t (Just (Packets 0)) soFar hpf):xs, rest, c) = do
  let c'= hpf c soFar
  (xs, rs, c')
update ((ParseOperator v t (Just (Packets l)) soFar hpf):xs, rest, c) = do

getLength :: [Int] -> (Length, [Int])
getLength [] = (Bits 0, [])
getLength 0:xs = do
  let (vs, rs) = splitAt 15 xs
  let n = bin2dec vs
  (Bits n, rs)
getLength 1:xs = do
  let (vs, rs) = splitAt 11 xs
  let n = bin2dec vs
  (Packets n, rs)

parse :: [Int] -> Context
parse xs = do
  let (_,_,result) = first (null.(\(_,x) -> x)) $ iterate update ([Start], xs, [])
  return result

main :: IO ()
main = do
  line <- join <$> map charToBin <$> getContents
  putStrLn $ show $ map bin2char line
