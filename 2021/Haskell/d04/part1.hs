import Prelude hiding (split)
import Data.Char
import Data.List.Index
import Data.List.Extra hiding (split)
import Relude.Extra.Tuple
import Control.Monad
import GHC.Utils.Misc (split)
import Data.Map (Map, fromList, member, lookup)
import qualified Data.Map as Map

type Calls = [Update]
type Board = [[Int]]
type Row = [Int]
type Coord = (Int, Int)

data BoardData = BoardData
  {
    board :: Board
  , index:: Map Int (Int, Int)
  , score :: Int
  , winningValue :: Int
  , winsOnTurn :: Int
  , columns :: [Int]
  , rows :: [Int]
  } deriving (Show)

data Update = Update {turn :: Int, value :: Int }

makeBoards :: [Row] -> [Board]
makeBoards rows = chunksOf 5 $ filter (not.null) rows

indexBoard :: Board -> Map Int Coord
indexBoard board = fromList $ join $ rearrange doubleIndexed
  where
    doubleIndexed = indexed $ fmap indexed board
    rearrange = fmap (\(r, row) -> fmap (\(c, v) -> (v, (r, c))) row )

prepareBoardData :: Board -> BoardData
prepareBoardData board = BoardData
  {
    board = board
  , index = indexBoard board
  , score = sum $ join board
  , winningValue = -1
  , winsOnTurn = -1
  , columns = take 5 $ repeat 0
  , rows = take 5 $ repeat 0
  }

update :: BoardData -> Update -> BoardData
update boardData (Update {turn = turn, value = value}) = maybe boardData (updateCoord value) coord
  where
    coord = Map.lookup value $ index boardData
    updateCoord :: Int -> Coord -> BoardData
    updateCoord x (r, c) = BoardData
      {
        board = board boardData
      , index = index boardData
      , score = score boardData - value
      , winningValue = if won then value else winningValue boardData
      , winsOnTurn = if won then turn else winsOnTurn boardData
      , columns = newColumns
      , rows = newRows
      }
      where
        won = (not $ isWinning boardData) && (any (>=5) $ newColumns ++ newRows)
        newColumns = modifyAt c (+ 1) $ columns boardData
        newRows = modifyAt r (+ 1) $ rows boardData

updateAll :: [BoardData] -> Update -> [BoardData]
updateAll boardDatas value = map (\boardData -> update boardData value) boardDatas

isWinning :: BoardData -> Bool
isWinning bd = winsOnTurn bd > -1

firstWinningBoard :: [BoardData] -> Calls -> [BoardData]
firstWinningBoard boardDatas calls = head $
  dropWhile (not.(any isWinning)) $
  scanl updateAll boardDatas calls

lastWinningBoard :: [BoardData] -> Calls -> [BoardData]
lastWinningBoard boardDatas calls = head $
  dropWhile (any (not.isWinning)) $
  scanl updateAll boardDatas calls

parseMatrix :: [String] -> [Row]
parseMatrix lines = map ((map read).words) lines

main :: IO ()
main = do
  first:rest <- lines <$> getContents
  let numbers = map read $ split ',' first
  let calls = map (\(idx, val) -> Update { turn = idx, value = val}) $ indexed numbers
  let matrix = parseMatrix rest
  let boards = makeBoards matrix
  let boardDatas = map prepareBoardData boards
  let part1Board = head $ filter isWinning $ firstWinningBoard boardDatas calls
  let part2Board = maximumOn winsOnTurn $ lastWinningBoard boardDatas calls
  putStrLn $ show $ (score part1Board) * (winningValue part1Board)
  putStrLn $ show $ (score part2Board) * (winningValue part2Board)
