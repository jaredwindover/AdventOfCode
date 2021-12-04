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
        let (x, y, _) = foldl handleCommand (0, 0, 0) commands
        putStrLn $ show $ (x * y)

      handleCommand (x, y, aim) (Forward value) = (x + value, y + (value * aim), aim)
      handleCommand (x, y, aim) (Up value) = (x, y, aim - value)
      handleCommand (x, y, aim) (Down value) = (x, y, aim + value)
