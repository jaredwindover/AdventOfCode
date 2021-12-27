module Main where

import Prelude hiding (lookup)
import Data.Char (digitToInt);
import Data.Bits (shift);
import Data.Map (Map, alter, empty, (!?), fromList)
import Data.Maybe (maybe, catMaybes, fromMaybe)
import Data.List (splitAt)
import Control.Monad (join)
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

getLength :: [Int] -> (Length, Int, [Int])
getLength [] = (Bits 0, 0, [])
getLength (0:xs) = do
  let (vs, rs) = splitAt 15 xs
  let n = bin2dec vs
  (Bits n, 16, rs)
getLength (1:xs) = do
  let (vs, rs) = splitAt 11 xs
  let n = bin2dec vs
  (Packets n, 12, rs)

tokenize :: Bool -> [Int] -> [Token]
tokenize _ [] = []
tokenize b s = do
  let (t, rest, inBody) = getToken b s
  t:(tokenize inBody rest)

getToken :: Bool -> [Int] -> (Token, [Int], Bool)
getToken True (i:a:b:c:d:rest) = do
  (BodyToken [a,b,c,d] 5, rest, i == 1)

getToken False (a:b:c:d:e:f:rest) = do
  let version = Version $ bin2dec [a,b,c]
  let t = Type $ bin2dec [d,e,f]
  if (t == Type 4)
    then (LiteralToken version 6, rest, True)
    else handleOperator version t rest
    where
      handleOperator v t s = do
        let (l, c, s') = getLength s
        (OperatorToken v t l $ 6 + c, s', False)
getToken _ xs = (EndToken $ length xs, [], False)

getVersion :: Token -> Int
getVersion (OperatorToken (Version v) t l _) = v
getVersion (LiteralToken (Version v) _) = v
getVersion _ = 0


justLiterals :: Token -> Bool
justLiterals (LiteralToken _ _) = True
justLiterals _ = False

justOperators :: Token -> Bool
justOperators (OperatorToken _ _ _ _) = True
justOperators _ = False

justBodies :: Token -> Bool
justBodies (BodyToken _ _) = True
justBodies _ = False

justLiteralsAndOperators :: Token -> Bool
justLiteralsAndOperators t = (justLiterals t) || (justOperators t)

type ConsumedBits = Int

data Type = Type Int deriving (Eq, Show)
data Version = Version Int deriving (Eq, Show)
data Length = Bits Int | Packets Int deriving (Eq, Show)
data Token = LiteralToken Version ConsumedBits  |
             OperatorToken Version Type Length ConsumedBits |
             BodyToken [Int] ConsumedBits |
             EndToken ConsumedBits deriving (Eq, Show)

data Expression = Num Int |
                  Sum [Expression] |
                  Product [Expression] |
                  Minimum [Expression] |
                  Maximum [Expression] |
                  GreaterThan [Expression] |
                  LessThan [Expression] |
                  EqualTo [Expression] deriving (Eq, Show)

evaluate :: Expression -> Int
evaluate (Num v) = v
evaluate (Sum es) = sum $ map evaluate es
evaluate (Product es) = product $ map evaluate es
evaluate (Minimum es) = minimum $ map evaluate es
evaluate (Maximum es) = maximum $ map evaluate es
evaluate (GreaterThan es) = do
  let v1 = evaluate $ es !! 0
  let v2 = evaluate $ es !! 1
  if (v1 > v2) then 1 else 0
evaluate (LessThan es) = do
  let v1 = evaluate $ es !! 0
  let v2 = evaluate $ es !! 1
  if (v1 < v2) then 1 else 0
evaluate (EqualTo es) = do
  let v1 = evaluate $ es !! 0
  let v2 = evaluate $ es !! 1
  if (v1 == v2) then 1 else 0

getBodyValue :: Token -> [Int]
getBodyValue (BodyToken v _) = v

buildOperator :: Type -> [Expression] -> Expression
buildOperator (Type 0) = Sum
buildOperator (Type 1) = Product
buildOperator (Type 2) = Minimum
buildOperator (Type 3) = Maximum
buildOperator (Type 5) = GreaterThan
buildOperator (Type 6) = LessThan
buildOperator (Type 7) = EqualTo


update :: (Int, [Expression], [Token]) -> (Int, [Expression], [Token])
update (cbs, es, ts) = do
  let (e, cbs', ts') = parse ts :: (Expression, ConsumedBits, [Token])
  (cbs + cbs', es ++ [e], ts')

parseToken :: (Token, [Token]) -> (Expression, ConsumedBits, [Token])
parseToken (LiteralToken _ cbs, rest) = do
  let (bodies, rest') = span justBodies rest
  let cbs' = sum $ map (\(BodyToken _ cbs') -> cbs') bodies
  let v = bin2dec $ join $ map getBodyValue bodies
  (Num v, cbs + cbs', rest')
parseToken (OperatorToken _ t (Bits k) cbs, rest) = do
  let (cbs', es, rest') = head $
        dropWhile (\(cbs',_,_) -> cbs' < k) $
        iterate update (0, [], rest)
  (buildOperator t es, cbs + cbs', rest')
parseToken (OperatorToken _ t (Packets k) cbs, rest) = do
  let (cbs', es, rest') = (iterate update (0, [], rest)) !! k
  (buildOperator t es, cbs + cbs', rest')


parse :: [Token] -> (Expression, ConsumedBits, [Token])
parse [] = error "No Tokens"
parse (t:ts) = do
  parseToken (t, ts)


main :: IO ()
main = do
  line <- join <$> map charToBin <$> getContents :: IO [Int]
  -- let line = join $ map charToBin $ "D8005AC2A8F0"
  let tokens = tokenize False line
  let (expression, _, _) = parse tokens
  let result = evaluate expression
  putStrLn $ show expression
  putStrLn $ show result

  -- putStrLn $ show $ sum $ map getVersion $ filter justLiteralsAndOperators tokens
