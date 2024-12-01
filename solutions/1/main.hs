module Main where

import Data.List (sort)

main :: IO ()
main = return ()

part1 :: IO ()
part1 = do
    input <- readFile "input.txt"
    let (column1, column2) = parseColumns input
        sortedCol1 = sort column1
        sortedCol2 = sort column2
        diffs = zipWith (-) sortedCol1 sortedCol2
        positiveDiffs = map abs diffs
        result = sum positiveDiffs
    print result

parseColumns :: String -> ([Int], [Int])
parseColumns input = 
    let rows = map (map read . words) (lines input)
        column1 = map head rows
        column2 = map last rows
    in (column1, column2)

part2 :: IO ()
part2 = do
    input <- readFile "input.txt"
    let (column1, column2) = parseColumns input
        freqs = map (\x -> x * (countItem x column1)) column2
        result = sum freqs
    print result

countItem :: Eq a => a -> [a] -> Int
countItem x xs = length (filter (== x) xs)