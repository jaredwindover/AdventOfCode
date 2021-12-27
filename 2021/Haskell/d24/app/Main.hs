module Main where

import Lib
import Prelude hiding (init)
import Data.Map (Map, fromList, insert, (!))
import Control.Monad (mapM_)
import Control.Monad.Loops (iterateUntilM)

data Register = W | X | Y | Z deriving (Eq, Ord)
instance Show Register where
  show W = "w"
  show X = "x"
  show Y = "y"
  show Z = "z"

data Operand = ROp Register | NOp Int deriving (Eq, Ord)
instance Show Operand where
  show (ROp r) = show r
  show (NOp n) = show n

data Storage = Storage (Map Register Int) deriving (Eq, Ord)
instance Show Storage where
  show s = "[" ++
    showRegisterValue W s ++ "," ++
    showRegisterValue X s ++ "," ++
    showRegisterValue Y s ++ "," ++
    showRegisterValue Z s ++
    "]"

showRegisterValue :: Register -> Storage -> String
showRegisterValue r s = show r ++ ": " ++ (show $ getValue (ROp r) s)

init :: Storage
init = Storage $ fromList [(W, 0), (X, 0), (Y, 0), (Z, 0)]

data Instruction = INP Operand |
                   ADD Operand Operand |
                   MUL Operand Operand |
                   DIV Operand Operand |
                   MOD Operand Operand |
                   EQL Operand Operand deriving (Eq, Ord, Show)

getDestination :: Instruction -> Register
getDestination (INP (ROp k)) = k
getDestination (ADD (ROp k) _) = k
getDestination (MUL (ROp k) _) = k
getDestination (DIV (ROp k) _) = k
getDestination (MOD (ROp k) _) = k
getDestination (EQL (ROp k) _) = k

getLeft :: Instruction -> Operand
getLeft (ADD a _) = a
getLeft (MUL a _) = a
getLeft (DIV a _) = a
getLeft (MOD a _) = a
getLeft (EQL a _) = a

getRight :: Instruction -> Operand
getRight (ADD _ b) = b
getRight (MUL _ b) = b
getRight (DIV _ b) = b
getRight (MOD _ b) = b
getRight (EQL _ b) = b

parseOperand :: String -> Operand
parseOperand "w" = ROp W
parseOperand "x" = ROp X
parseOperand "y" = ROp Y
parseOperand "z" = ROp Z
parseOperand n = NOp $ read n

parse :: [String] -> Instruction
parse ["inp", r] = INP $ parseOperand r
parse ["add", r0, r1] = ADD (parseOperand r0) (parseOperand r1)
parse ["mul", r0, r1] = MUL (parseOperand r0) (parseOperand r1)
parse ["div", r0, r1] = DIV (parseOperand r0) (parseOperand r1)
parse ["mod", r0, r1] = MOD (parseOperand r0) (parseOperand r1)
parse ["eql", r0, r1] = EQL (parseOperand r0) (parseOperand r1)

getValue :: Operand -> Storage -> Int
getValue (NOp x) _ = x
getValue (ROp y) (Storage s) = s ! y

type Stack = (Instruction, String, Storage)

showStack :: Stack -> String
showStack (i, inp, s) = (rightPad 40 $ show s) ++ (rightPad 10 $ show i) ++ (show inp)

pad :: Int -> String -> String
pad n s = if (l < n) then (take (n - l) $ repeat ' ') else ""
  where l = length s

leftPad :: Int -> String -> String
leftPad n s = (pad n s) ++ s

rightPad :: Int -> String -> String
rightPad n s = s ++ (pad n s)

evaluate' :: (Storage, String, [Stack]) -> Instruction -> (Storage, String, [Stack])
evaluate' (storage, input, stack) i = do
  let storage' = updateStorage i input storage
  let input' = updateInput i input
  (storage', input', (i, input', storage'): stack)
  where
    updateStorage :: Instruction -> String -> Storage -> Storage
    updateStorage i@(INP a@(ROp k)) input (Storage storage) =
      Storage $ insert k (read $ [input !! 0]) storage
    updateStorage i input s@(Storage storage) = do
      let destination = getDestination i
      let leftV = getValue (getLeft i) s
      let rightV = getValue (getRight i) s
      let newValue = updateStorage' i leftV rightV
      Storage $ insert destination newValue storage

    updateStorage' :: Instruction -> Int -> Int -> Int
    updateStorage' (ADD _ _) a b = a + b
    updateStorage' (MUL _ _) a b = a * b
    updateStorage' (DIV _ _) a b = a `div` b
    updateStorage' (MOD _ _) a b = a `mod` b
    updateStorage' (EQL _ _) a b = if (a == b) then 1 else 0

    updateInput :: Instruction -> String -> String
    updateInput (INP (ROp r)) input = drop 1 input
    updateInput _ input = input

evaluate :: [Instruction] -> String -> IO [Stack]
evaluate instructions input = do
  return $ (\(_, _, x) -> x) $ foldl evaluate' (init, input, []) (instructions)


test :: [Instruction] -> IO ()
test instructions = do
  -- let toTest = take 1 $ filter (not.(elem '0')) $  map (show) [x | x <- [99999999999999, 99999999999998..11111111111111]]
  --let toTest = take 100 $ filter (not.(elem '0')) $  map (show) [x | x <- [22222222222222..99999999999999]]
  --            _____*_**_****
  mapM_ test' ["56999951829399"]
    where
      test' input = do
        evaluations <- evaluate instructions input
        putStrLn $ unlines $ map showStack $ reverse evaluations
        -- let result = (evaluations ! Z) == 0
        -- putStrLn $ "Input: " ++ input ++ " Result: " ++ show result


main :: IO ()
main = do
  instructions <- map (parse.words) <$> lines <$> getContents
  test instructions
