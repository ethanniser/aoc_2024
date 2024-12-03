part1 :: IO ()
part1 = do
  input <- readFile "input.txt"
  print $ length $ lines input