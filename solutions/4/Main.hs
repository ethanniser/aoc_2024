import Data.Function (on)
import Data.List (groupBy, isPrefixOf, sortOn, transpose)

diagonals :: [[a]] -> [[a]]
diagonals matrix =
    let indexedElements = [(i + j, val) | (i, row) <- zip [0 ..] matrix, (j, val) <- zip [0 ..] row]
        groupedByDiagonal = groupBy ((==) `on` fst) $ sortOn fst indexedElements
     in map (map snd) groupedByDiagonal

searchForXMAS :: String -> Int
searchForXMAS s = inner 0 s
  where
    inner acc s@(c : cs)
        | "XMAS" `isPrefixOf` s = inner (acc + 1) cs
        | otherwise = inner acc cs
    inner acc "" = acc

runSearchForRows :: [String] -> Int
runSearchForRows = sum . map searchForXMAS

-- 12
-- 34

part1 :: IO ()
part1 = do
    input <- readFile "test.txt"
    let lines' = lines input
    let linesReversed = map reverse lines'
    let cols = transpose lines'
    let colsReversed = map reverse cols
    let d1 = diagonals lines'
    let d2 = diagonals $ reverse lines'
    let d3 = diagonals cols
    let d4 = diagonals $ reverse cols
    let result = sum $ map runSearchForRows [lines', linesReversed, cols, colsReversed, d1, d2, d3, d4]
    print result

part2 :: IO ()
part2 = do
    input <- readFile "test.txt"
    print "hi"
