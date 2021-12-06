main = do
  nums <- map (map read) <$> lines <$> getContents
  putStrLn $ foldr1 (zipWith (+)) nums
