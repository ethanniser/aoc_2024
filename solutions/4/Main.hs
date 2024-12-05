import Data.Function (on)
import Data.List (groupBy, intercalate, isPrefixOf, permutations, sortOn, transpose)

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

getPermutations :: String -> [[String]]
getPermutations input = [lines', linesReversed, cols, colsReversed, d1, d2, d3, d4]
  where
    lines' = lines input
    linesReversed = map reverse lines'
    cols = transpose lines'
    colsReversed = map reverse cols
    d1 = diagonals lines'
    d2 = diagonals $ reverse lines'
    d3 = diagonals cols
    d4 = diagonals $ map reverse cols

printPermutations :: [[String]] -> IO ()
printPermutations = mapM_ (putStrLn . unwords)

part1 :: IO ()
part1 = do
    input <- readFile "input.txt"
    let result = sum $ map searchForXMAS $ concat $ getPermutations input
    print result

validCorners :: (Char, Char, Char, Char) -> Bool
validCorners (topLeft, topRight, bottomLeft, bottomRight) = or [one, two, three, four]
  where
    one = and [topLeft == 'M', topRight == 'M', bottomLeft == 'S', bottomRight == 'S']
    two = and [topLeft == 'M', topRight == 'S', bottomLeft == 'M', bottomRight == 'S']
    three = and [topLeft == 'S', topRight == 'M', bottomLeft == 'S', bottomRight == 'M']
    four = and [topLeft == 'S', topRight == 'S', bottomLeft == 'M', bottomRight == 'M']

isValidX :: [[Char]] -> Bool
isValidX x
    | x !! 1 !! 1 == 'A' = validCorners (head (head x), head x !! 2, head (x !! 2), x !! 2 !! 2)
    | otherwise = False

get3by3Blocks :: [[Char]] -> [[[Char]]]
get3by3Blocks grid =
    [ [[grid !! (i + x) !! (j + y) | y <- [0 .. 2]] | x <- [0 .. 2]]
    | i <- [0 .. length grid - 3]
    , j <- [0 .. length (head grid) - 3]
    ]

part2 :: IO ()
part2 = do
    input <- readFile "input.txt"
    let lines' = lines input
    let result = sum $ map (fromEnum . isValidX) $ get3by3Blocks lines'
    print result
