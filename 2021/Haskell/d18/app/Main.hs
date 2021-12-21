module Main where

import Lib
import Control.Exception
import Control.Monad
import Data.Char

data Elem = RegElem Int | PairElem Elem Elem deriving (Eq, Show)

elemToString :: Elem -> String
elemToString (RegElem x) = show x
elemToString (PairElem l r) = "[" ++ (elemToString l) ++ "," ++ (elemToString r) ++ "]"

parseOneElem :: String -> (Elem, String)
parseOneElem (v:xs)
  | '0' <= v && v <= '9' = (RegElem $ (ord v) - (ord '0'), xs)
parseOneElem (_:xs) = do
  let (p, rest) = parseOneElem xs
  let (_:xs') = assert (rest /= []) rest
  let (p', rest') = parseOneElem xs'
  let (_:xs'') = assert (rest' /= []) rest'
  (PairElem p p', xs'')

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys

applyExplodeR :: (Maybe Int) -> Elem -> Elem
applyExplodeR Nothing e = e
applyExplodeR (Just v) (RegElem ev) = RegElem (v + ev)
applyExplodeR mv@(Just v) (PairElem l r) = PairElem (applyExplodeR mv l) r

applyExplodeL :: (Maybe Int) -> Elem -> Elem
applyExplodeL Nothing e = e
applyExplodeL (Just v) (RegElem ev) = RegElem (v + ev)
applyExplodeL mv@(Just v) (PairElem l r) = PairElem l (applyExplodeL mv r)

reduceExplode' :: (Int, Elem) -> (Bool, (Maybe Int, Maybe Int), Elem)
reduceExplode' (_, e@(RegElem _)) = (False, (Nothing, Nothing), e)
reduceExplode' (4, (PairElem (PairElem _ _) _)) = error "Too deeply nested pair"
reduceExplode' (4, (PairElem _ (PairElem _ _))) = error "Too deeply nested pair"
reduceExplode' (4, (PairElem (RegElem l) (RegElem r))) = (True, (Just l, Just r), RegElem 0)
reduceExplode' (c, e@(PairElem l r)) = do
  let (b, (ml, mr), l') = reduceExplode' (c + 1, l)
  if (b) then (True, (ml, Nothing), PairElem l' $ applyExplodeR mr r) else do
    let (b', (ml, mr), r') = reduceExplode' (c + 1, r)
    if b' then (True, (Nothing, mr), PairElem (applyExplodeL ml l) r') else do
      (False, (Nothing, Nothing), e)

reduceExplode :: Elem -> (Bool, Elem)
reduceExplode e = do
  let (b, _, e') = reduceExplode' (0, e)
  (b, e')

reduceSplit :: Elem -> (Bool, Elem)
reduceSplit e@(RegElem v)
  | v > 9 = do
      let l = v `div` 2
      let r = v - l
      (True, PairElem (RegElem l) (RegElem r))
  | otherwise = (False, e)
reduceSplit e@(PairElem l r) = do
  let (b, l') = reduceSplit l
  if b then (True, PairElem l' r) else do
    let (b', r') = reduceSplit r
    if b' then (True, PairElem l r') else (False, e)


reduce :: Elem -> Elem
reduce e = do
  let (b, e') = reduceExplode e
  if b then e' else do
    let (b', e'') = reduceSplit e
    if b' then e'' else e


add' :: Elem -> Elem -> Elem
add' l r = PairElem l r

add :: Elem -> Elem -> Elem
add l r = converge (==) $ iterate reduce $ add' l r

mag :: Elem -> Int
mag (RegElem v) = v
mag (PairElem l r) = (3*(mag l)) + (2*(mag r))

parse :: String -> Elem
parse s = do
  fst $ parseOneElem s

main :: IO ()
main = do
  pairs <- map parse <$> lines <$> getContents
  let pairs' = [(a, b) | a <- pairs, b <- pairs, (a /= b)]
  let max = maximum $ map mag $ map (\(a, b) -> add a b) pairs'
  putStrLn $ show $ max
  -- putStrLn $ show $ mag $ foldl1 add pairs
