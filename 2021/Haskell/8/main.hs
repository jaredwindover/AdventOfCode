import Prelude hiding (lookup)
import Data.List.Split
import Data.List hiding (lookup)
import Control.Monad
import Data.Set (Set, fromList, intersection, unions, member, elems)
import Data.Map (Map, adjust, alter, empty, assocs, lookup, findWithDefault)
import qualified Data.Map (fromList)
import Data.Maybe (maybe, mapMaybe)

data Position = A | B | C | D | E | F | G deriving (Eq, Ord, Show)
data Number = N0 | N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 deriving (Eq, Ord, Show)
type Index = Map (Int, Int, Bool) Position

zero = fromList [A, B, C, E, F, G]
one = fromList [C, F]
two = fromList [A, C, D, E, G]
three = fromList [A, C, D, F, G]
four = fromList [B, C, D, F]
five = fromList [A, B, D, F, G]
six = fromList [A, B, D, E, F, G]
seven = fromList [A, C, F]
eight = fromList [A, B, C, D, E, F, G]
nine = fromList [A, B, C, D, F, G]

fromSet :: [Position] -> Char
fromSet x
  | sx == zero = '0'
  | sx == one = '1'
  | sx == two = '2'
  | sx == three = '3'
  | sx == four = '4'
  | sx == five = '5'
  | sx == six = '6'
  | sx == seven = '7'
  | sx == eight = '8'
  | sx == nine = '9'
  | otherwise = 'x'
  where sx = fromList x

numbers = [N0, N1, N2, N3, N4, N5, N6, N7, N8, N9]

getPositions :: Number -> Set Position
getPositions N0 = zero
getPositions N1 = one
getPositions N2 = two
getPositions N3 = three
getPositions N4 = four
getPositions N5 = five
getPositions N6 = six
getPositions N7 = seven
getPositions N8 = eight
getPositions N9 = nine

getCount :: Position -> Int
getCount A = 8
getCount B = 6
getCount C = 8
getCount D = 7
getCount E = 4
getCount F = 9
getCount G = 7

handleInput :: Index -> ([String], [String]) -> IO Int
handleInput index (samples, result) = do
  let mapping = buildMapping index samples
  let cypher = Data.Map.fromList mapping
  let cyphered = map (\n -> map (\l -> findWithDefault A l cypher) n) result
  let converted = map fromSet cyphered
  let res = read converted :: Int
  return res;
  where
    letterCount = foldl (\m -> \c -> alter (\mv -> Just $ maybe 1 (+1) mv) c m) empty $ join samples
    buildMapping index samples = join $ map (\w -> mapMaybe (\(l, idx) -> fmap (\v -> (l, v)) (lookup idx index)) $ map (\l -> (l, (length w, findWithDefault 0 l letterCount, elem l w))) ['a'..'g']) samples

buildIndex :: (Ord b, Eq b) => (a -> b) -> (a -> c) -> [a] -> Map b [c]
buildIndex f1 f2 l = foldl (\m -> \a -> alter (\mv -> Just $ maybe [f2 a] ((f2 a):) mv) (f1 a) m ) empty l

main = do
  inputs <- map (\[x, y] -> (words x, words y)) <$> map (splitOn " | ") <$> lines <$> getContents
  answer <- sum <$> mapM (handleInput finalLookup) inputs
  putStrLn $ show answer
  where index n p = (n, p, length $ getPositions n, getCount p, member p $ getPositions n)
        listIndex = join $ map (\n -> map (\p -> index n p) $ elems eight) numbers
        builtIndex = buildIndex (\(_,_,a,b,c) -> (a,b,c)) (\(a,b,_,_,_) -> (a,b)) listIndex
        finalLookup = Data.Map.fromList $ map (\(l, [(n, p)]) -> (l,p))$ filter (\(_,r) -> length r == 1) $ assocs builtIndex
