module Command
  ( Command (Forward, Up, Down)
  , parseCommand
  ) where

data Command = Forward(Int) | Up(Int) | Down(Int) deriving (Show)

parseCommand :: String -> Int -> Maybe Command
parseCommand "forward" x = Just $ Forward x
parseCommand "up" x = Just $ Up x
parseCommand "down" x = Just $ Down x
parseCommand _ _ = Nothing
