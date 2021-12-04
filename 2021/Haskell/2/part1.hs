import Data.Char
import Command

readInput :: IO (Maybe [Command])
readInput = do
  byLine <- lines <$> getContents
  let commands = map words byLine
  return $ sequence $ map parse commands
    where parse ([x,y]) = parseCommand x $ read y

main = do
  commands <- readInput
  handleCommands commands
    where
      handleCommands ::  Maybe [Command] -> IO ()
      handleCommands Nothing = do
        putStrLn "Error!"
      handleCommands (Just commands) =  do
        let (x, y) = foldl handleCommand (0, 0) commands
        putStrLn $ show $ (x * y)
      handleCommand (x, y) (Forward value) = (x + value, y)
      handleCommand (x, y) (Up value) = (x, y - value)
      handleCommand (x, y) (Down value) = (x, y + value)
