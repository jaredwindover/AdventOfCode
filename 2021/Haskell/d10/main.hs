import Control.Monad (mapM)
import Data.Set (Set, fromList, member)
import Data.List (sort)

data Result = Good | Bad Char deriving (Show)

open :: Set Char
open = fromList ['(', '[', '{', '<']

close :: Set Char
close = fromList [')', ']', '}', '>']

closer :: Char -> Char
closer '(' = ')'
closer '[' = ']'
closer '<' = '>'
closer _ = '}'

isGood :: Result -> Bool
isGood Good = True
isGood _ = False

scoreResult :: Result -> Int
scoreResult Good = 0
scoreResult (Bad ')') = 3
scoreResult (Bad ']') = 57
scoreResult (Bad '}') = 1197
scoreResult (Bad '>') = 25137

score :: [Char] -> IO Int
score cs = do
  let bads = dropWhile isGood $ map fst $ scanl update (Good, []) cs
  if length bads == 0 then
    return 0
  else
    return $ scoreResult $ head bads
  where
    update :: (Result, [Char]) -> Char -> (Result, [Char])
    update (_, s) c
      | member c open = (Good, (closer c):s)
      | (member c close) && (c == (s !! 0) )= (Good, drop 1 s)
      | member c close = (Bad c, s)
      | otherwise = (Bad c, s)


scoreIncomplete :: [Char] -> IO Int
scoreIncomplete cs = do
  putStrLn $ show cs
  let result = foldl update (Good, []) cs
  putStrLn $ show result
  if not $ isGood $ fst result then
    return (-1)
  else
    return $ foldl accScore 0 $ snd result
  where
    update :: (Result, [Char]) -> Char -> (Result, [Char])
    update (soFar, s) c
      | not $ isGood soFar = (soFar, s)
      | member c open = (Good, (closer c):s)
      | (member c close) && (c == (s !! 0) )= (Good, drop 1 s)
      | member c close = (Bad c, s)
      | otherwise = (Bad c, s)

    accScore :: Int -> Char -> Int
    accScore x ')' = (x * 5) + 1
    accScore x ']' = (x * 5) + 2
    accScore x '}' = (x * 5) + 3
    accScore x '>' = (x * 5) + 4
    accScore x _ = -1


main = do
  ls <- lines <$> getContents
  scores <- sort <$> filter (>=0) <$> mapM scoreIncomplete ls
  putStrLn $ show $ scores !! (length scores) `div` 2
